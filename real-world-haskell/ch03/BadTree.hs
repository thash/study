-- このコードは, "ふたつのTreeが等しい"という意味にはなら*ない*.
bad_nodesAreSame (Node a _ _) (Node a _ _) = Just a
bad_nodesAreSame _            _            = Nothing

-- 怒られます.
-- ch03/BadTree.hs:2:24:
--     Conflicting definitions for `a'
--     Bound at: ch03/BadTree.hs:2:24
--               ch03/BadTree.hs:2:37
--     In an equation for `bad_nodesAreSame'
--
-- ch03/BadTree.hs:2:32: Not in scope: data constructor `Node'
-- Failed, modules loaded: none.
