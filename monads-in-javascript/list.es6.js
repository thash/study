function* fibonacci() {
    let [prev, curr] = [0, 1];
    for (;;) {
        [prev, curr] = [curr, prev + curr];
        yield curr;
    }
}

// "use strict";
//
// var marked0$0 = [fibonacci].map(regeneratorRuntime.mark);
// function fibonacci() {
//     var prev, curr, _ref;
//
//     return regeneratorRuntime.wrap(function fibonacci$(context$1$0) {
//         while (1) switch (context$1$0.prev = context$1$0.next) {
//             case 0:
//                 prev = 0;
//                 curr = 1;
//
//             case 2:
//                 _ref = [curr, prev + curr];
//                 prev = _ref[0];
//                 curr = _ref[1];
//                 context$1$0.next = 7;
//                 return curr;
//
//             case 7:
//                 context$1$0.next = 2;
//                 break;
//
//             case 9:
//             case "end":
//                 return context$1$0.stop();
//         }
//     }, marked0$0[0], this);
// }


// 実行は単に node で叩くだけではだめ. こうなる
// ReferenceError: regeneratorRuntime is not defined
//
// babel-runtime が必要
