data TreeNode a = TreeNode {
  left  :: Maybe (TreeNode a),
  right :: Maybe (TreeNode a),
  value :: a
} deriving Show

treeByLevels :: Maybe (TreeNode a) -> [a]
treeByLevels = bfs [] . (:[])

bfs :: [a] -> [Maybe (TreeNode a)] -> [a]
bfs res [] = res
bfs res ts = bfs (res ++ foldr getNode [] ts) (concat $ map getTrees ts)
  where
    getNode Nothing ns = ns
    getNode (Just (TreeNode _ _ v)) ns = v:ns

    getTrees Nothing = []
    getTrees (Just (TreeNode l Nothing v)) = [l]
    getTrees (Just (TreeNode Nothing r v)) = [r]
    getTrees (Just (TreeNode l r v)) = [l, r]