// without class
function Identity(value) {
  this.value = value
}

// this を参照したいのでアロー関数は使わない
Identity.prototype.bind = function (transform) {
  return transform(this.value)
}

Identity.prototype.toString = function () {
  return 'Identity(' + this.value + ')'
}

var result =
  new Identity(5).bind(value =>
    new Identity(6).bind(value2 =>
      new Identity(value + value2)))

console.log(result.toString())


// ---------------------------------------
// with class
class Id {
  constructor(value) {
    this.value = value
  }

  bind(transform) {
    return transform(this.value)
  }

  toString() {
    return 'Id(' + this.value + ')'
  }
}

var result =
  new Id(5).bind(value =>
    new Id(6).bind(value2 =>
      new Id(value + value2)))

console.log(result.toString())
