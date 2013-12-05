package main

import (
  "fmt"
  "math"
)

type Vertex struct {
	X, Y float64
}

// GoにはClassがないが, 特定のstructに対してmethodを定義することができる
func (v *Vertex) Abs() float 64 {
	return math.Sqrt(v.X*v.X + v.Y*v.Y)
}

// まあ任意の型でもokなんだけど
type MyFloat float64
func (f MyFloat) Abs2() float64 {
   // ...
}

// interface定義はtypeに追記, メソッド名のみ記述
type Abser interface {
	Abs() float64
}

type Reader interface {
	Read(b []byte) (n int, err error)
}

// interfaceの組み合わせ
type SuperReader interface {
        Abser
        Reader
}


func main() {
	v := &Vertex{3,4}
        fmt.Println(v.Abs()) // OOP的に呼び出し
}
