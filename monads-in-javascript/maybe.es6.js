// Maybe モナドは Id と違い, 空の値を伝搬する

// Just は wrap のみ. 基本 id と変わらない
class Just {
  constructor(value) {
    this.value = value
  }

  bind(transform) {
    return transform(this.value)
  }

  toString() {
    return 'Just(' + this.value + ')'
  }
}

var Nothing = {
  bind() { return this },
  toString() { return 'Nothing' }
}
console.log(Nothing.toString())

var result =
  new Just(5).bind(value =>
    Nothing.bind(value2 =>
      new Just(value + alert(value2))))

console.log(result.toString()) // => show 'Nothing'. no alert. つか node だとエラー

// if (user != null)
// みたいな null チェックが冗長だと思うなら Maybe モナドの出番

// 穏当に(Nothingによる連鎖中止なく)足される使い方
var result2 =
  new Just(5).bind(value =>
    new Just(3).bind(value2 =>
      new Just(value + value2)))

console.log(result2.toString())
