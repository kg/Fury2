#if defined(_MSC_VER) && defined(_DEBUG)

#include <map>
#include <crtdbg.h>
#include <windows.h>
#include <dbghelp.h>
//#include <dbgint.h>  (CODE FROM INTERNAL dbgint.h BELOW)
#include "Win32Memory.h"

/// BEGIN CODE FROM DBGINT.H

/*
* For diagnostic purpose, blocks are allocated with extra information and
* stored in a doubly-linked list.  This makes all blocks registered with
* how big they are, when they were allocated, and what they are used for.
*/

#define nNoMansLandSize 4

typedef struct _CrtMemBlockHeader
{
        struct _CrtMemBlockHeader * pBlockHeaderNext;
        struct _CrtMemBlockHeader * pBlockHeaderPrev;
        char *                      szFileName;
        int                         nLine;
#ifdef _WIN64
        /* These items are reversed on Win64 to eliminate gaps in the struct
        * and ensure that sizeof(struct)%16 == 0, so 16-byte alignment is
        * maintained in the debug heap.
        */
        int                         nBlockUse;
        size_t                      nDataSize;
#else  /* _WIN64 */
        size_t                      nDataSize;
        int                         nBlockUse;
#endif  /* _WIN64 */
        long                        lRequest;
        unsigned char               gap[nNoMansLandSize];
        /* followed by:
        *  unsigned char           data[nDataSize];
        *  unsigned char           anotherGap[nNoMansLandSize];
        */
} _CrtMemBlockHeader;

#define pbData(pblock) ((unsigned char *)((_CrtMemBlockHeader *)pblock + 1))
#define pHdr(pbData) (((_CrtMemBlockHeader *)pbData)-1)

/// END CODE FROM DBGINT.H


namespace pyr {

    /// Information stored with a live allocation.
    struct Request {
        CallStack stack;
        const char* filename;
        int line;
    };

    typedef std::map< long, Request, std::less<long>,
                      Win32Allocator< std::pair<long, Request> > >
            RequestSet;
    typedef RequestSet::const_iterator RequestSetCIter;

    struct LeakCheckerState : Win32Object {
        Mutex mutex;
    
        // Set of live allocations.
        RequestSet requests;

        // Old allocation hook, so we can restore it after dumping leaks.    
        _CRT_ALLOC_HOOK oldHook;
    };
    
    LeakCheckerState* _state;
    // Probably need a mutex to lock _state.
    
    
    static int __cdecl hook(
        int allocType, void* data, size_t size, int blockType,
        long request, const unsigned char* filename, int line)
    {
        if (blockType == _CRT_BLOCK) {
            return TRUE;
        }
        
        ScopedLock lock__(_state->mutex);
        
        // Allow re-entrancy.  Allocations called from the hook don't count.
        _CRT_ALLOC_HOOK this_hook = _CrtSetAllocHook(_state->oldHook);
        // Need a scope guard here.  :(
            
        try {
            
            switch (allocType) {
                case _HOOK_ALLOC: {
                    Request r;  // Implicitly performs a stack trace.
                    r.filename = reinterpret_cast<const char*>(filename);
                    r.line     = line;

                    PYR_ASSERT(_state->requests.count(request) == 0, "Two allocations have the same request number");
                    _state->requests[request] = r;
                    break;
                }

                case _HOOK_FREE: {
                    // The _CrtSetAllocHook documention is WRONG, so we have to
                    // set the parameters ourselves.
                    _CrtMemBlockHeader* header = pHdr(data);
                    size     = header->nDataSize;
                    request  = header->lRequest;
                    filename = reinterpret_cast<unsigned char*>(header->szFileName);
                    line     = header->nLine;
                    
                    // This assert triggers far too often to be useful.
                    //PYR_ASSERT(_state->requests.count(request) == 1, "Free without allocation");
                    _state->requests.erase(request);
                    break;
                }
            }

            _CrtSetAllocHook(this_hook);
            return TRUE;
        }
        catch (const std::exception&) {
            // We could use PYR_LOG(), but it might throw too.
            _CrtSetAllocHook(this_hook);
            return FALSE;
        }
        #if 0  // Catch everything?
        catch (...) {
        }
        #endif
    }

    static void __cdecl dumpLeaks() {
        PYR_ASSERT(_state, "LeakCheckerState not valid.  dumpLeaks called before registerLeakChecker?");
    
        //_CrtDumpMemoryLeaks();
        _CrtSetAllocHook(_state->oldHook);
        
        // Delete the LeakCheckerState when we're done.
        struct Sentry {
            ~Sentry() {
                delete _state;
                _state = 0;
            }
        } sentry__;

        const RequestSet& req = _state->requests;

        std::ostringstream os;
        os << "Leaked blocks: " << req.size() << '\n';
        for (RequestSetCIter i = req.begin(); i != req.end(); ++i) {
            const char* filename = i->second.filename;
            filename = filename ? filename : "";
            int line = i->second.line;
            os << "Filename: " << filename << " -- Line: " << line << "\n";
            os << i->second.stack.asString();
            os << "\n";
        }
        OutputDebugString(os.str().c_str());
    }

    void registerLeakChecker() {
        PYR_ASSERT(!_state, "LeakCheckerState already set.  registerLeakChecker called twice?");
        _state = new LeakCheckerState;
        _state->oldHook = _CrtSetAllocHook(hook);
        
        // Do memory leak checks at program exit.
        atexit(dumpLeaks);

        // Don't do the default (mostly useless) leak report.
        int dbg = _CrtSetDbgFlag(_CRTDBG_REPORT_FLAG);
        dbg &= ~_CRTDBG_LEAK_CHECK_DF;
        _CrtSetDbgFlag(dbg);
    }

}

#else

namespace pyr {

    void registerLeakChecker() {
        // no-op
    }

}

#endif