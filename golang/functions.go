package main

import (
  "fmt"
//  "math"
)

// 引数の型, 戻り値の型
func add(x int, y int) int {
  return x + y
}

// 複数値を返す
func swap(x, y string) (string, string) {
	return y, x
}

// named results -- 返す変数名を予め指定, 暗黙return可能
func split(sum int) (x, y int) { // intなxとyを返す, と宣言
	x = sum * 4 / 9
        y = sum - x
        return
}

// Goの関数はクロージャである
func adder() func(int) int {
	sum := 0
        return func(x int) int {
		sum += x
                return sum
        }
}

func main() {
        fmt.Println(add(32,13))
	a,b := swap("Hello", "world") // 代入は:=

        // 第一級関数. 関数を変数に代入できる
	hypot := func(x, y float64) float64 {
	         return math.Sqrt(x*x y*y)
	}

        pos, neg := adder(), adder()
        for i := 0; i < 10; i++ {
            fmt.Println(
		    pos(i),
                    neg(-2*i),
	    )
        }
}
