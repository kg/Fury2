Export extern void* MemAllocate(unsigned Count);
Export extern void MemDeallocate(void* Ptr);

//template <class T>
//class alloc {
//public:
//  // type definitions
//  typedef T        value_type;
//  typedef T*       pointer;
//  typedef const T* const_pointer;
//  typedef T&       reference;
//  typedef const T& const_reference;
//  typedef std::size_t    size_type;
//  typedef std::ptrdiff_t difference_type;
//
//  // rebind allocator to type U
//  template <class U >
//  struct rebind {
//    typedef alloc< U > other;
//  };
//
//  // return address of values
//  pointer address (reference value) const {
//    return &value;
//  }
//  const_pointer address (const_reference value) const {
//    return &value;
//  }
//
//  /* constructors and destructor
//  * - nothing to do because the allocator has no state
//  */
//  alloc() throw() {
//  }
//  alloc(const alloc& src) throw() {
//  }
//  template <class U >
//  alloc (const alloc< U > &src) throw() {
//  }
//  ~alloc() throw() {
//  }
//
//  // return maximum number of elements that can be allocated
//  size_type max_size () const throw() {
//    return INT_MAX / sizeof(T);
//  }
//
//  // allocate but don't initialize num elements of type T
//  pointer allocate (size_type num, const void* = 0) {
//    // print message and allocate memory with global new
//    pointer ret = (pointer)MemAllocate(num * sizeof(T));
//    return ret;
//  }
//
//  // initialize elements of allocated storage p with value value
//  void construct (pointer p, const T& value) {
//    memset(p, 0, sizeof(T));
//    memcpy(p, &value, sizeof(T));
//    //// initialize memory with placement new
//    //new(p) T;
//  }
//
//  // destroy elements of initialized storage p
//  void destroy (pointer p) {
//    // destroy objects by calling their destructor
//    p->~T();
//  }
//
//  // deallocate storage p of deleted elements
//  void deallocate (pointer p, size_type num) {
//    memset(p, 0, num * sizeof(T));
//    MemDeallocate(p);
//  }
//};
//
//// return that all specializations of this allocator are interchangeable
//template <class T1, class T2>
//bool operator== (const alloc<T1>&, const alloc<T2>&) throw() {
//  return true;
//}
//template <class T1, class T2>
//bool operator!= (const alloc<T1>&, const alloc<T2>&) throw() {
//  return false;
//}

class PoolAllocated {
public:
  //void* operator new (size_t size) {
  //  return MemAllocate(size);
  //}
  //void operator delete (void *p) {
  //  MemDeallocate(p);
  //}
};