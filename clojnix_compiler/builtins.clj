(ns clojnix-compiler.builtins)


(def simple-builtins
  {'string? "builtins.isString"
   'boolean? "builtins.isBool"
   'vector? "builtins.isList"
   'int? "builtins.isInt"
   'nil? "builtins.isNull"
   'map? "builtins.isAttrs"
   'first "builtins.head"
   'second "(a: builtins.elemAt a 1)"
   'inc "(a: a + 1)"
   'dec "(a: a - 1)"
   'rest "builtins.tail"
   'nth "builtins.elemAt"
   '* "(a: b: a * b)"
   '+ "(a: b: a + b)"
   '= "(a: b: a == b)"
   'not= "(a: b: a != b)"
   '< "(a: b: a < b)"
   '> "(a: b: a > b)"
   '<= "(a: b: a <= b)"
   '>= "(a: b: a >= b)"
   'concat "(a: b: a ++ b)"
   'and "(a: b: a && b)"
   'or "(a: b: a || b)"
   'throw "(a: builtins.throw a)"
   'map "(a: b: builtins.map a b)"
   'get-flake "builtins.getFlake"
   'if "(a: b: c: if a then b else c)"
   'flatten "(a: flatten a)"
   'merge "(a: b: a // b)"
   'slurp "(a: import a)"
   'subs "(target: start: end: builtins.substring start end target)"
   'get-attr "(attr: aset: builtins.getAttr attr aset)"
   'constantly "(a: a)"
   })

(def inline-attr-by-path-expr
  "(attrPath: default: set:
    let
      lenAttrPath = builtins.length attrPath;
      attrByPath' =
        n: s:
        (
          if n == lenAttrPath then
            s
          else
            (
              let
                attr = builtins.elemAt attrPath n;
              in
              if s ? ${attr} then attrByPath' (n + 1) s.${attr} else default
            )
        );
    in
    attrByPath' 0 set)")

(def flatten-expr
  "(let this = x: if builtins.isList x then builtins.concatMap (y: this y) x else [ x ]; in this)")

(def builtins
  (merge simple-builtins
         {'attr-by-path inline-attr-by-path-expr
          'flatten flatten-expr
          }))
