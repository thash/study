package main
import "fmt"

func sum(a []int, c chan int) { // c がchannel
	sum := 0
	for _, v := range a {
		sum += v
	}
	// sumをチャネルcへ送る
        c <- sum
}


// > デフォルトでは, 片方が準備できるまで送受信はブロックされます.
// > これは, 明確なロックや条件変数がなくても, goroutineの同期を許します.
func main() {
	a := []int{7, 2, 8, -9, 4, 0}

        // mapとsliceのように, 使う前にchannelをmake
        // 第二引数に数値を入れるとチャネルのバッファ長さを定義できる
	c := make(chan int)
	go sum(a[:len(a)/2], c)
	go sum(a[len(a)/2:], c)

	// cから受信し, 変数をx,yに割り当てる
	x, y := <-c, <-c

	// 送り手からの「もうデータ無いよ」という情報を表すために受信式に2ツ目パラメータ
	v, ok := <-c // mapへのアクセスの時と同じ

	fmt.Println(x, y, x+y)
}


func fibonacci(c, quit chan int) {
	x, y := 0, 1
	for {
		// selectで受信メッセージのパターンマッチ? Erlangにも似た機能があった
		select {
		case c <- x:
			x, y = y, x+y
		case <-quit:
			fmt.Println("quit")
			return
		}
	}
}
