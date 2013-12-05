package main
import (
  "fmt"
  "net/http"
)

type Hello struct{}

// Web serverを書く
func (h Hello) ServeHTTP(
	w http.ResponseWriter,
	r *http.Request) {
	fmt.Fprint(w, "Hello")
}

func main() {
        var h Hello
	http.ListenAndServe("localhost:4000", h)
	// $ curl http://localhost:4000/
        // Hello
}
