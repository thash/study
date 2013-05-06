require("./book");

// imaginary new as a method, not as an operator.
Function.method('new', function() {
  var that = Object.create(this.prototype);
  var other = this.apply(that, arguments);
  return (typeof other === 'object' && other) || that;
});


var Mammal = function(name) {
  this.name = name;
};

Mammal.prototype.get_name = function() {
  return this.name;
};

var myM = new Mammal('heyhey');
console.log(myM.name);
console.log(myM.get_name());


var Cat = function(name) {
  this.name = name;
};

Cat.prototype = new Mammal();

// [imaginary] how JS treats scope in block
var block = function() {
  var oldScope = scope;
  scope = Object.create(scope);
  advance('{');
  parse(scope);
  advance('}');
  scope = oldScope;
};


// functional approach
// constructor, in lower case.
var mammal = function(spec) {
  // spec is something like env
  var that = {};

  that.get_name = function() {
    return spec.name;
  };

  that.says = function () {
    return spec.saying || '';
  };

  return that;
};

var cat = function(spec) {
  spec.saying = spec.saying || 'meow';
  var that = mammal(spec);
  that.purr = function(n) {
    var i, s = '';
    for (i = 0; i < n; i += 1) {
      if (s) {
        s += '-';
      }
      s += 'r';
    }
    return s;
  };
  that.get_name = function() {
    return that.says() + ' ' + spec.name + ' ' + that.says();
  };
  return that;
};

var myCat = cat({name: 'Helvetica'});

// define something like 'super'
Object.method('superior', funciton(name) {
  var that = this;
  method = that[name];
  return function() {
    return method.apply(that, arguments);
  };
});

