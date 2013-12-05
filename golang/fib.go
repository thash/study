package main
import "fmt"

// 面白いFibonacciの解き方.
func fib() func() int{
        x,y,z := 0,1,0
	return func() int {
        	z,x,y = x,y,x+y
                return z
	}
}

func main() {
	f := fib()
        for i := 0; i < 15; i++ {
		fmt.Println(f())
	}
}
