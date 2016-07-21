"use strict";

Object.defineProperty(exports, "__esModule", {
    value: true
});
exports.queryFields2 = exports.queryFieldsWithFragments = exports.queryFields = exports.serverUrl = undefined;

var _fableCore = require("fable-core");

var _fable_inject = require("./fable_inject.js");

require("isomorphic-fetch");

var serverUrl = exports.serverUrl = "http://localhost:8083";
var queryFields = exports.queryFields = "{ id, name, appearsIn, friends { name } }";
var queryFieldsWithFragments = exports.queryFieldsWithFragments = "{ ...data, friends { name } } fragment data on Human { id, name, appearsIn }";

(function (arg00) {
    _fableCore.Async.startImmediate(arg00);
})(function (builder_) {
    return builder_.delay(function (unitVar) {
        return builder_.bind(function () {
            var id = "1000";
            return function () {
                var cont = function (value) {
                    return value;
                };

                return function (query) {
                    return (0, _fable_inject.graphqlLaunchQuery)("http://localhost:8083", "hero", cont, query);
                };
            }()(function (argValues) {
                return (0, _fable_inject.graphqlBuildQuery)("hero", "{ ...data, friends { name } } fragment data on Human { id, name, appearsIn }", ["id"], argValues);
            }([id]));
        }(), function (_arg1) {
            var hero = _arg1;

            if (hero != null) {
                var hero_1 = hero;
                (function () {
                    var clo1 = _fableCore.String.fsFormat("My hero is %A")(function (x) {
                        console.log(x);
                    });

                    return function (arg10) {
                        clo1(arg10);
                    };
                })()(hero_1.name);
                (function () {
                    var clo1 = _fableCore.String.fsFormat("Appears in %O: %b")(function (x) {
                        console.log(x);
                    });

                    return function (arg10) {
                        var clo2 = clo1(arg10);
                        return function (arg20) {
                            clo2(arg20);
                        };
                    };
                })()("Empire")(hero_1.appearsIn.some(function () {
                    var x = "Empire";
                    return function (y) {
                        return x === y;
                    };
                }()));

                _fableCore.String.fsFormat("My hero's friends are:")(function (x) {
                    console.log(x);
                });

                Array.from(_fableCore.Seq.choose(function (x) {
                    return x.name;
                }, hero_1.friends)).forEach(function () {
                    var clo1 = _fableCore.String.fsFormat("- %s")(function (x) {
                        console.log(x);
                    });

                    return function (arg10) {
                        clo1(arg10);
                    };
                }());
                return builder_.zero();
            } else {
                return builder_.zero();
            }
        });
    });
}(_fableCore.Async));

var queryFields2 = exports.queryFields2 = "{ id, name";
//# sourceMappingURL=query.js.map