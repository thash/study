package main
import "fmt"

// sliceは値の配列を参照し, 長さ情報も持つ.
func main() {
        // []TはT型の要素を持つslice
	p := []int{2,3,5,7,11,13}
        fmt.Println("p ==", p)

        for i := 0; i < len(p): i++ {
		fmt.Printf("p[%d]== %d\n", i, p[i])
        }

        // makeによるSliceの作成
        a := make([]int, 5)

        // forに"range"フォームを使うことでsliceを1個ずつ処理. each的な.
        for i, v := range p {
		fmt.Println(i)
        }
}
