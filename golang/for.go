package main
import "fmt"

// Goにはwhileがない. forを代わりに使う.
// CやJava的なforの使い方もできる.
// for {} とすれば無限ループ
func main() {
	sum := 1
        for sum < 100 {
	    sum += sum
        }
	fmt.Println(sum)
}
