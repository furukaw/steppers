## (型無しλ計算 + shift/reset + 整数 + 整数の足し算) のステッパ

### 構文
```
v := λx.e                  (λ抽象)
   | n                     (整数)

e := v
   | x                     (変数)
   | e e                   (関数適用)
   | e + e                 (整数の加算)
   | shift (fun k -> e)    (継続の取得)
   | reset (fun () -> e)   (継続の限定)
```

### small-step semantics
```
(λx.e) v  〜>  e[v/x]
n1 + n2  〜>  n  (ただし n = n1 + n2)
reset (fun () -> F[shift (fun k -> e)])  〜>  reset (fun () -> (λk.e) (λx.reset (fun () -> F[x]))
reset (fun () -> v)  〜>  v
```
