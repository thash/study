````haskell
# (3.14, 2.71) ;;
- : float * float = (3.14, 2.71)
```

> 組の型は要素の型を`*`でつないだものになります

つまり全体として「`float * float`」型.

```haskell
# let add pair = match pair with
       (a, b) -> a + b ;;

val add : int * int -> int = <fun>

# add (4, 5) ;;
- : int = 9
```

`int * int`の組（つまり(3,4)とかそういうの）を渡すとintを返す関数addを定義してる.

同様に, 最初からpairを引数として受け取ることも出来る

```haskell
# let add2 (a,b) = a + b ;;
val add2 : int * int -> int = <fun>
# add2 (4, 5) ;;
- : int = 9
```
