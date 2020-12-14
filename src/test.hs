module Test where

data CL s
  = Skip
  | Update (s -> s) (CL s)
  | If (s -> Bool) (CL s) (CL s) (CL s)
  | While (s -> Bool) (CL s) (CL s)

data S = S {x :: Int, y :: Int}

example1 :: CL S
example1 =
  Update
    (\s -> S (x s) 0)
    ( While
        (\s -> x s > 0)
        ( Update
            (\s -> S (x s) (y s + x s))
            ( Update
                (\s -> S (x s - 1) (y s))
                Skip
            )
        )
        Skip
    )

skip :: CL s
skip = Skip

update :: (s -> s) -> CL s
update f = Update f skip

cond :: (s -> Bool) -> CL s -> CL s -> CL s
cond c p q = If c p q skip

while :: (s -> Bool) -> CL s -> CL s
while c p = While c p skip

fol :: CL s -> CL s -> CL s
Skip `fol` k = k
Update f p `fol` k = Update f (p `fol` k)
If c p q r `fol` k = If c p q (r `fol` k)
While c p q `fol` k = While c p (q `fol` k)

example1' =
  update (\s -> S (x s) 0)
    `fol` while
      (\s -> x s > 0)
      ( update (\s -> S (x s) (y s + x s))
          `fol` update (\s -> S (x s - 1) (y s))
      )

data CLF s a
  = SkipF
  | UpdateF (s -> s) a
  | IfF (s -> Bool) a a a
  | WhileF (s -> Bool) a a

c :: CLF s (CL s) -> CL s
c SkipF = Skip
c (UpdateF f p) = Update f p
c (IfF c p q r) = If c p q r
c (WhileF c p q) = While c p q
