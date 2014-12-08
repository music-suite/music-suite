
through :: Applicative f => Lens' s a -> Lens s t a b -> Lens (f s) (f t) (f a) (f b)
through lens1 lens2 =
  lens getBP (flip setBP)
  -- (\sa sbt afb s -> sbt s <$> afb (sa s)) getBP (flip setBP)
  -- (\sbt afb s -> sbt s <$> afb (getBP s)) (flip setBP)
  -- (\afb s -> (flip setBP) s <$> afb (getBP s))
  -- \afb s -> (flip setBP) s <$> afb (getBP s)
  -- \afb s -> (flip $ \x a -> liftA2 (lens2 .~) x a) s <$> afb (getBP s)
  -- \afb s -> (\a x -> liftA2 (lens2 .~) x a) s <$> afb (getBP s)
  -- \afb s -> (\x -> liftA2 (lens2 .~) x s) <$> afb (getBP s)
  -- \afb s -> (\x -> liftA2 (lens2 .~) x s) <$> afb ((^. lens1) <$> s)
  -- \afb s -> (\x -> liftA2 (lens2 .~) x s) <$> afb ((\s -> s ^. lens1) <$> s)
  -- \afb s -> (\x -> liftA2 (lens2 .~) x s) <$> afb ((\s -> getConst (lens1 Const s)) <$> s)
  -- \afb s -> (\x -> liftA2 (\s -> set lens2 s) x s) <$> afb ((\s -> getConst (lens1 Const s)) <$> s)
  -- \afb s -> (\x -> liftA2 (\b ->  runIdentity .
  --        lens2 (\_ -> Identity b)) x s) <$> afb ((\s -> getConst (lens1 Const s)) <$> s)

  -- \afb s -> (\x -> liftA2 (\b ->  runIdentity . lens2 (\_ -> Identity b)) x s)
  --       <$>
  --       afb ((\s -> getConst (lens1 Const s)) <$> s)

  -- \f s -> (\x -> (\b ->  runIdentity . lens2 (const $ Identity b)) <$> x <*> s)
  --       <$>
  --       f ((\s -> getConst (lens1 Const s)) <$> s)

  -- \f s -> (\x -> liftA2 (\a b -> runIdentity $ (lens2 . const . Identity $ b) a) s x)
  --       <$>
  --       f ((getConst . lens1 Const) <$> s)

  -- \f s -> liftA2 ( \a b -> runIdentity (lens2 (const (Identity b)) a) ) s <$> (f ((getConst . lens1 Const) <$> s))
  -- \f s -> liftA2 ( \a -> runIdentity . flip lens2 a . const . Identity ) s <$> (f ((getConst . lens1 Const) <$> s))
  -- \f s -> liftA2 (\a -> runIdentity . (`lens2` a) . const . Identity) s <$> f (getConst <$> lens1 Const <$> s)
  where
    getBP = fmap (view lens1)
    setBP = liftA2 (set lens2)
{-# INLINE through #-}
