template<class character> class Char {
};

template<> class Char<char> {
public:
  static unsigned length(const char* data) {
    if (!data)
      return 0;
    return strlen(data);
  }

  static unsigned compare(const char* lhs, const char* rhs) {
    if (!lhs)
      return -1;
    if (!rhs)
      return 1;
    return strcmp(lhs, rhs);
  }

  static unsigned icompare(const char* lhs, const char* rhs) {
    if (!lhs)
      return -1;
    if (!rhs)
      return 1;
    return stricmp(lhs, rhs);
  }

  static unsigned size() {
    return sizeof(char);
  }

  static void copy(char* dest, const char* source, unsigned count) {
    if (!dest)
      return;
    if (!source)
      return;
    memcpy(dest, source, count * sizeof(char));
  }

  template<class type> static void copy_convert(char* dest, const type* source, unsigned count) {
    if (!dest)
      return;
    if (!source)
      return;
    unsigned i = count;
    while (i--) {
      *dest = (type)(*source);
      dest++;
      source++;
    }
  }

  static void terminate(char* dest, unsigned length) {
    if (!dest) 
      return;
    dest[length] = 0;
  }
};

template<> class Char<wchar_t> {
public:
  static unsigned length(const wchar_t* data) {
    if (!data)
      return 0;
    return wcslen(data);
  }

  static unsigned compare(const wchar_t* lhs, const wchar_t* rhs) {
    if (!lhs)
      return -1;
    if (!rhs)
      return 1;
    return wcscmp(lhs, rhs);
  }

  static unsigned icompare(const wchar_t* lhs, const wchar_t* rhs) {
    if (!lhs)
      return -1;
    if (!rhs)
      return 1;
    return wcsicmp(lhs, rhs);
  }

  static unsigned size() {
    return sizeof(wchar_t);
  }

  static void copy(wchar_t* dest, const wchar_t* source, unsigned count) {
    if (!dest)
      return;
    if (!source)
      return;
    memcpy(dest, source, count * sizeof(wchar_t));
  }

  template<class type> static void copy_convert(wchar_t* dest, const type* source, unsigned count) {
    if (!dest)
      return;
    if (!source)
      return;
    unsigned i = count;
    while (i--) {
      *dest = (type)(*source);
      dest++;
      source++;
    }
  }

  static void terminate(wchar_t* dest, unsigned length) {
    if (!dest) 
      return;
    dest[length] = 0;
  }
};

template<class character> class String {
public:
  character* Data;
  unsigned Length;

  String()
    :Data(0),
     Length(0)
  {
    Allocate(0);
  }

  String(unsigned length) 
    :Data(0),
     Length(length)
  {
    Allocate(length);
  }

  String(const character* data) 
    :Data(0),
     Length(0)
  {
    int len = Char<character>::length(data);
    Allocate(len);
    Char<character>::copy(Data, data, len);
  }

  String(const character* data, unsigned length) 
    :Data(0),
     Length(0)
  {
    Allocate(length);
    Char<character>::copy(Data, data, length);
  }

  template<class type> String(const type* data) 
    :Data(0),
     Length(0)
  {
    int len = Char<type>::length(data);
    Allocate(len);
    Char<character>::copy_convert<type>(Data, data, len);
  }

  template<class type> String(const type* data, unsigned length) 
    :Data(0),
     Length(0)
  {
    Allocate(length);
    Char<character>::copy_convert<type>(Data, data, length);
  }

  template<class type> String(const String<type>& other) 
    :Data(0),
     Length(0)
  {
    Allocate(other.Length);
    Char<character>::copy_convert<type>(Data, other.Data, other.Length);
  }

  String(const String<character>& other)
    :Data(0),
     Length(0)
  {
    Allocate(other.Length);
    Char<character>::copy(Data, other.Data, other.Length);
  }

  String(const String<character>& other, unsigned start, unsigned count)
    :Data(0),
     Length(0)
  {
    unsigned l = other.Length - start;
    unsigned c = min(l, count);
    Allocate(c);
    Char<character>::copy(Data, other.pointer(start), c);
  }

  ~String() {
    Deallocate();
  }

  void Allocate(unsigned count) {
    unsigned bytes = (count+1) * Char<character>::size();
//    Data = new character[count+1];
    Data = (character*)MemAllocate(bytes);
    memset(Data, 0, bytes);
    Length = count;
  }

  void Reallocate(unsigned count, bool preserve) {
    if (preserve) {
      unsigned oldLength = Length;
      character* oldData = Data;
      Allocate(count);
      if (oldData) {
        Char<character>::copy(Data, oldData, min(oldLength, Length));
        MemDeallocate(oldData);
      }
    } else {
      Deallocate();
      Allocate(count);
    }
  }

  void Deallocate() {
    if (Data) {
      MemDeallocate(Data);
      Length = 0;
      Data = 0;
    }
  }

  void Extend(unsigned count) {
    Reallocate(Length + count, true);
  }

  void Erase() {
    Reallocate(0, false);
  }

  void Append(const String<character>& text) {
    Extend(text.Length);
    Set(Length - text.Length, text);
  }

  void Set(unsigned index, const String<character>& text) {
    Set(index, text, 0, text.Length);
  }

  void Set(unsigned index, const String<character>& text, unsigned sourceIndex, unsigned count) {
    Char<character>::copy(pointer(index), text.pointer(sourceIndex), min(count, text.Length));
  }

  unsigned Find(character value) const {
    return Find(0, value);
  }

  unsigned Find(unsigned index, character value) const {
    for (unsigned i = index; i < Length; i++) {
      if ((*this)[i] == value)
        return i;
    }
    return -1;
  }

  bool StartsWith(const String<character>& text) const {
    if (text.Length > Length)
      return false;
    unsigned c = text.Length;
    for (unsigned i = 0; i < c; i++) {
      if (text[i] != (*this)[i])
        return false;
    }
    return true;
  }

  bool EndsWith(const String<character>& text) const {
    if (text.Length > Length)
      return false;
    unsigned c = text.Length;
    unsigned o = Length - text.Length;
    for (unsigned i = 0; i < c; i++) {
      if (text[i] != (*this)[i+o])
        return false;
    }
    return true;
  }

  bool StartsWithI(const String<character>& text) const {
    if (text.Length > Length)
      return false;
    unsigned c = text.Length;
    for (unsigned i = 0; i < c; i++) {
      if (tolower(text[i]) != tolower((*this)[i]))
        return false;
    }
    return true;
  }

  bool EndsWithI(const String<character>& text) const {
    if (text.Length > Length)
      return false;
    unsigned c = text.Length;
    unsigned o = Length - text.Length;
    for (unsigned i = 0; i < c; i++) {
      if (tolower(text[i]) != tolower((*this)[i+o]))
        return false;
    }
    return true;
  }

  bool Equals(const String<character>& text) const {
    int result = Char<character>::compare(this->Data, text.Data);
    return result == 0;
  }

  bool EqualsI(const String<character>& text) const {
    int result = Char<character>::icompare(this->Data, text.Data);
    return result == 0;
  }

  inline character* pointer() const {
    return Data;
  }

  inline const character* pointer_const() const {
    return Data;
  }

  inline character* pointer(unsigned index) const {
    return Data + index;
  }

  unsigned size() const {
    return Length;
  }

  character* begin() const {
    return Data;
  }

  character* end() const {
    return Data + Length;
  }

  inline character& operator[](unsigned index) {
    assert((index >= 0) && (index <= Length));
    return *pointer(index);
  }

  inline const character& operator[](unsigned index) const {
    assert((index >= 0) && (index <= Length));
    return *pointer(index);
  }

  inline const String<character>& operator+=(const String<character>& rhs) {
    Append(rhs);
    return *this;
  }

  inline const String<character>& operator=(const String<character>& rhs) {
    if ((&rhs) == (this))
      return *this;
    Deallocate();
    Allocate(rhs.Length);
    Set(0, rhs);
    return *this;
  }

  inline bool operator<(const String<character>& rhs) const {
    int result = Char<character>::compare(this->Data, rhs.Data);
    return result < 0;
  }

  inline bool operator<=(const String<character>& rhs) const {
    int result = Char<character>::compare(this->Data, rhs.Data);
    return result <= 0;
  }

  inline bool operator==(const String<character>& rhs) const {
    int result = Char<character>::compare(this->Data, rhs.Data);
    return result == 0;
  }

  inline bool operator!=(const String<character>& rhs) const {
    int result = Char<character>::compare(this->Data, rhs.Data);
    return result != 0;
  }

  inline bool operator>=(const String<character>& rhs) const {
    int result = Char<character>::compare(this->Data, rhs.Data);
    return result >= 0;
  }

  inline bool operator>(const String<character>& rhs) const {
    int result = Char<character>::compare(this->Data, rhs.Data);
    return result > 0;
  }

  std::vector<String<character> >* Split(character delimiter) const {
    std::vector<String<character> > *items = new std::vector<String<character> >();
    int position = 0;
    int next_position = Find(position, delimiter);
    if (next_position != -1) {
      while (next_position != -1) {
        if ((next_position - position) <= 0) {
//          items->push_back(String<character>());
        } else {
          items->push_back(String<character>(pointer(position), next_position - position));
        }
        position = next_position + 1;
        next_position = Find(position, delimiter);
      }
      next_position = Length;
      items->push_back(String<character>(pointer(position), next_position - position));
    } else {
      items->push_back(String<character>(*this));
    }
    return items;
  }

  String<character> ToLower() const {
    String<character> out(Length);
    for (unsigned i = 0; i < Length; i++) {
      out[i] = tolower((*this)[i]);
    }
    return out;
  }

  String<character> ToUpper() const {
    String<character> out(Length);
    for (unsigned i = 0; i < Length; i++) {
      out[i] = toupper((*this)[i]);
    }
    return out;
  }
};

template<class character, class elem, class traits> std::basic_ostream<elem, traits>& operator << (std::basic_ostream<elem, traits>& os, const String<character>& string)
{
  os << string.Data;
  return os;
}

template<class character, class elem, class traits> std::basic_ostream<elem, traits>& operator << (std::basic_ostream<elem, traits>& os, String<character>* string)
{
  if (string)
    os << string->Data;
  return os;
}

typedef String<wchar_t> WString;
typedef String<char> AString;