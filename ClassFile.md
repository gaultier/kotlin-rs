## Example: main with one variable declaration

`Foo.java`:

```java
class Foo {
    public static void main(String[] args) {
        int res = 1 + 2;
    }
}
```

`hexdump -C Foo.class` output:

```
00000000  ca fe ba be 00 00 00 39  00 0f 0a 00 02 00 03 07  |.......9........|
00000010  00 04 0c 00 05 00 06 01  00 10 6a 61 76 61 2f 6c  |..........java/l|
00000020  61 6e 67 2f 4f 62 6a 65  63 74 01 00 06 3c 69 6e  |ang/Object...<in|
00000030  69 74 3e 01 00 03 28 29  56 07 00 08 01 00 03 46  |it>...()V......F|
00000040  6f 6f 01 00 04 43 6f 64  65 01 00 0f 4c 69 6e 65  |oo...Code...Line|
00000050  4e 75 6d 62 65 72 54 61  62 6c 65 01 00 04 6d 61  |NumberTable...ma|
00000060  69 6e 01 00 16 28 5b 4c  6a 61 76 61 2f 6c 61 6e  |in...([Ljava/lan|
00000070  67 2f 53 74 72 69 6e 67  3b 29 56 01 00 0a 53 6f  |g/String;)V...So|
00000080  75 72 63 65 46 69 6c 65  01 00 0a 68 65 6c 6c 6f  |urceFile...hello|
00000090  2e 6a 61 76 61 00 20 00  07 00 02 00 00 00 00 00  |.java. .........|
000000a0  02 00 00 00 05 00 06 00  01 00 09 00 00 00 1d 00  |................|
000000b0  01 00 01 00 00 00 05 2a  b7 00 01 b1 00 00 00 01  |.......*........|
000000c0  00 0a 00 00 00 06 00 01  00 00 00 01 00 09 00 0b  |................|
000000d0  00 0c 00 01 00 09 00 00  00 1f 00 01 00 02 00 00  |................|
000000e0  00 03 06 3c b1 00 00 00  01 00 0a 00 00 00 0a 00  |...<............|
000000f0  02 00 00 00 03 00 02 00  04 00 01 00 0d 00 00 00  |................|
00000100  02 00 0e                                          |...|
00000103
```

`javap -v Foo.class` output:

```
Classfile /private/tmp/Foo.class
  Last modified 1 Mar 2020; size 259 bytes
  SHA-256 checksum 3fab74da17bc3c362573de53a5b1ce96d7431764278fcbc95188c65ddc9e5a22
  Compiled from "hello.java"
class Foo
  minor version: 0
  major version: 57
  flags: (0x0020) ACC_SUPER
  this_class: #7                          // Foo
  super_class: #2                         // java/lang/Object
  interfaces: 0, fields: 0, methods: 2, attributes: 1
Constant pool:
   #1 = Methodref          #2.#3          // java/lang/Object."<init>":()V
   #2 = Class              #4             // java/lang/Object
   #3 = NameAndType        #5:#6          // "<init>":()V
   #4 = Utf8               java/lang/Object
   #5 = Utf8               <init>
   #6 = Utf8               ()V
   #7 = Class              #8             // Foo
   #8 = Utf8               Foo
   #9 = Utf8               Code
  #10 = Utf8               LineNumberTable
  #11 = Utf8               main
  #12 = Utf8               ([Ljava/lang/String;)V
  #13 = Utf8               SourceFile
  #14 = Utf8               hello.java
{
  Foo();
    descriptor: ()V
    flags: (0x0000)
    Code:
      stack=1, locals=1, args_size=1
         0: aload_0
         1: invokespecial #1                  // Method java/lang/Object."<init>":()V
         4: return
      LineNumberTable:
        line 1: 0

  public static void main(java.lang.String[]);
    descriptor: ([Ljava/lang/String;)V
    flags: (0x0009) ACC_PUBLIC, ACC_STATIC
    Code:
      stack=1, locals=2, args_size=1
         0: iconst_3
         1: istore_1
         2: return
      LineNumberTable:
        line 3: 0
        line 4: 2
}
SourceFile: "hello.java"
```

- Magic number: bytes 0..4: `0xca 0xfe 0xba 0xbe`
- Minor version: bytes 4..6: `0x00 0x00`
- Major version: bytes 6..8: `0x00 0x39` => 57 i.e java 12
- Constant pool count: bytes 8..10: `0x00 0x0f` => 15 constants
- Constant pool: 
  - Tag: bytes 10..11: `0x0a` => 10 i.e `MethodRef`
  - Class index: bytes 11..13: `0x00 0x02` => 2 i.e the class name is located at the index 2 in the constant pool
  - Name and type index: bytes 13..15: `0x00 0x03` => 3 i.e the name and type info is located at the index 3 in the constant pool
  
  - Tag: bytes 15..16: `0x07` => 7 i.e `Class`
  - Name index: bytes 16..18: `0x00 0x04` => 4 i.e the name is located at the index 4 in the constant pool

  - Tag: bytes 18..19: `0x0c` => 12 i.e `Name and type`
  - Name index: 19..21: `0x00 0x05` => 5 i.e the name is located at the index 5 in the constant pool
  - Descriptor index: bytes 21..23: `0x00 0x06` => 6 i.e the descriptor is located at the index 6 in the constant pool

  - Tag: bytes 23..24: `0x01` => 1 i.e `Utf8`
  - Length: bytes 24..26: `0x00 0x10` => 16 i.e the UTF-8 string to come has a length of 16
  - Content: bytes 26..42: `0x6a 0x61 0x76 0x61 0x2f 0x6c 0x61 0x6e 0x67 0x2f 0x4f 0x62 0x6a 0x65 0x63 0x74` => `java/lang/Object`

  - Tag: bytes 42..43: `0x01` => 1 i.e `Utf8`
