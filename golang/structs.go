package main

import "fmt"

// 構造体を定義
type Vertex struct {
  X int
  Y int
}

func main() {
	v := Vertex{1,2}
        v.X = 4 // 構造体のフィールドにアクセスするのはドット(.)
        q := &v // ポインタ
	fmt.Println(Vertex{1,2})
}
