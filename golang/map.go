package main
import "fmt"

type Vertex struct {
	Lat, Long float64 // Goは数値系の型が細かく用意されている
}

// mapはRubyで言うHash.
// Bell Labs, Googleという文字列がkey.
var m = map[string]Vertex{
	"Bell Labs": Vertex{
		40.68443, -74.39967,
	},
        "Google": Vetext{
		37.42202, -122.08408,
	},
}

// アクセス
m["Google"]
// 削除
delete(m, "Google")
// 存在確認
elem, ok = m["Google"] // 存在すればokにはtrueが入る
