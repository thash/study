package main
import "fmt"

// 変数の宣言
// 宣言したが使わない変数があるばあいコンパイルエラーになる
var x, y, z int

// 定数. := は使えない
const Pi = 3.14

func main() {
        // いっぺんに初期化
        var xx, yy, zz int = 1, 2, 3
	c, python, java := true, false, "no" // := は暗黙の宣言
}
