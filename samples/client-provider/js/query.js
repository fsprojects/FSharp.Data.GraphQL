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
    _fableCore.Async.startImmediate(arg00, null);
})(function (builder_) {
    return builder_.delay(function (unitVar) {
        var id;
        return builder_.bind((id = "1000", function () {
            return function () {
                var cont;
                return cont = function (value) {
                    return value;
                }, function (query) {
                    return (0, _fable_inject.graphqlLaunchQuery)("http://localhost:8083", "hero", cont, query);
                };
            }();
        }()(function (argValues) {
            return (0, _fable_inject.graphqlBuildQuery)("hero", "{ ...data, friends { name } } fragment data on Human { id, name, appearsIn }", ["id"], argValues);
        }([id]))), function (_arg1) {
            var hero, hero_1;
            return hero = _arg1, hero != null ? (hero_1 = hero, function () {
                return function () {
                    var clo1;
                    return clo1 = _fableCore.String.fsFormat("My hero is %A")(function (x) {
                        console.log(x);
                    }), function (arg10) {
                        clo1(arg10);
                    };
                }();
            }()(hero_1.name), function () {
                return function () {
                    var clo1;
                    return clo1 = _fableCore.String.fsFormat("Appears in %O: %b")(function (x) {
                        console.log(x);
                    }), function (arg10) {
                        return function () {
                            var clo2;
                            return clo2 = clo1(arg10), function (arg20) {
                                clo2(arg20);
                            };
                        }();
                    };
                }();
            }()("Empire")(hero_1.appearsIn.some(function () {
                var x;
                return x = "Empire", function (y) {
                    return x === y;
                };
            }())), _fableCore.String.fsFormat("My hero's friends are:")(function (x) {
                console.log(x);
            }), Array.from(_fableCore.Seq.choose(function (x) {
                return x.name;
            }, hero_1.friends)).forEach(function () {
                var clo1;
                return clo1 = _fableCore.String.fsFormat("- %s")(function (x) {
                    console.log(x);
                }), function (arg10) {
                    clo1(arg10);
                };
            }()), builder_.zero(null)) : (null, builder_.zero(null));
        });
    });
}(_fableCore.Async));

var queryFields2 = exports.queryFields2 = "{ id, name";
//# sourceMappingURL=query.js.map