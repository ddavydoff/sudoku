import Data.List
import Debug.Trace

-------------------------------------------

type Mat2d a = [[a]]
type MatInt2d = Mat2d Int
type Mat3d a = [[[a]]]
type MatInt3d = Mat3d Int
type ListM a = [a]
type ListMInt =ListM Int
type Pair a = (a,a)
type PairInt = Pair Int
type ListP a = [(a,a)]
type ListPInt = ListP Int

-------------------------------------------
-- Constant
n  =  9::Int
nm =  3::Int  
nk = 81::Int 

-------------------------------------------
-- IO function

intArray :: Int -> IO [[Int]]
intArray 0 = return []
intArray x = do
    str <- getLine
    nextInt <- intArray (x - 1)
    let int = map read (words str)::[Int]
    return (int:nextInt)

addSpaces :: Int -> String
addSpaces 0 = ""
addSpaces n = " " ++ addSpaces (n-1) 
    
show_2d_ :: ListMInt -> String
show_2d_ rs = foldr (++) "" (map (\s -> "      " ++ show s  ++ "") rs)

print_mat2d :: [[Int]] -> String
print_mat2d x = foldr (++) "" (map (\s -> (show_2d_ s) ++ "\n")  x)

show_2d :: MatInt2d -> String
show_2d x = foldr (++) "" (map (\x -> addSpaces (n-length(x)) ++ x) (map (\xx -> foldr (++) "" (map show xx) ++ " ") x))

print_mat3d :: MatInt2d -> String
print_mat3d [] = ""
print_mat3d x = (show_2d  (take n x)) ++ "\n" ++ (print_mat3d (drop n x))   
    
-------------------------------------------
-- Conversion function

convert_matrix_to_stroke_list :: MatInt2d -> ListPInt
convert_matrix_to_stroke_list x 
        = map (\(i,j)->(i*n+j,(x !! i) !! j)) sort
         where indx  = [(i,j) | i<-[0..n-1],j<-[0..n-1]]
               sort  = filter (\ (ii,jj) ->  0 /= (x !! ii) !! jj) indx

convert_matrix3d_to_stroke_list :: MatInt2d -> ListPInt
convert_matrix3d_to_stroke_list x 
        = map (\i->(i,(x !! i) !! 0)) sort
         where indx  = [i | i<-[0..nk-1]]
               sort  = filter (\ ii ->  length(x !! ii)==1) indx

convert_matrix3d_to_matrix :: MatInt2d -> MatInt2d
convert_matrix3d_to_matrix mat 
        = [[ if (length(mat!!(conv_p2l i j))==1) then head (mat!!(conv_p2l i j)) else 0 | i<-[0..n-1]]|j<-[0..n-1] ]

conv_p2l = \x -> \y -> x*n+y
conv_l2p = \x -> (divn x,modn x)
fx = \x -> \p -> x+3*((get_quad_l p) `div` nm)
fy = \x -> \p -> x+3*((get_quad_l p) `mod` nm)
get_quad = \(x,y) -> (x `div` nm)*nm + y `div` nm
get_quad_l = \p -> (\(x,y) -> (x `div` nm)*nm + y `div` nm )(conv_l2p p)
divn = \p -> p `div` n
modn = \p -> p `mod` n 

---------------------------------------------------
-- Function opeartion with matrix

init_table = replicate nk [1,2..9]

gen_stroke_list_wl:: Int -> [Int]
gen_stroke_list_wl p =  [ i*n+(p `mod` n)| i<-[0..n-1],i/=(divn p) ]++
                        [ (p `div` n)*n+i| i<-[0..n-1],i/=(modn p) ]++
                        [ conv_p2l (fx i p) (fy j p) | i<-[0..nm-1],j<-[0..nm-1],((fx i p)/=(divn p))||((fy j p)/=(modn p)) ]

delete_from_c__::MatInt2d->(Int,Int)->[[Int]]
delete_from_c__ t3d (zz,zp) = [if i==zp then (delete zz (t3d!!i)) else (t3d!!i) | i<-[0..nk-1] ]

delete_from_c_::MatInt2d->[(Int,Int)]->[[Int]]
delete_from_c_ t3d [] = t3d
delete_from_c_ t3d (x:xs) = delete_from_c_ (delete_from_c__ t3d x) xs

set_to_c__::MatInt2d->(Int,Int)->[[Int]]
set_to_c__ t3d (zz,zp) = [if i==zp then [zz] else (t3d!!i)|i<-[0..nk-1] ]

add_cell::MatInt2d->(Int,Int)->[[Int]]
add_cell t3d (zp,zz) = delete_from_c_ (set_to_c__ t3d (zz,zp)) (map   (\x -> (zz,x)) (gen_stroke_list_wl zp))

add_cell_list::[[Int]]->[(Int,Int)]->[[Int]]
add_cell_list t3d [] = t3d
add_cell_list t3d (x:xs) = add_cell_list (add_cell t3d x) xs

is_correct::MatInt2d->Bool
is_correct x = foldr (&&) True (map (\x -> length(x)/=0) x)

is_solve::MatInt2d->Bool
is_solve x =foldr (&&) True (map (\x -> length(x)==1) x)

---------------------------------------------------
-- Main Function 

solve_::MatInt2d->Int->[Int]->MatInt2d
solve_ t3d ind [] = t3d
solve_ t3d ind (x:xs) = if is_solve(t3d5)==True then t3d5 else solve_ t3d ind xs
                        where
                                t3d4 =  (add_cell_list t3d [(ind,x)])
                                f4 = convert_matrix3d_to_stroke_list t3d4
                                t3d5 = solve t3d4 f4

solve::MatInt2d->[(Int,Int)]->MatInt2d
solve t3d listm = if is_correct(t3d3) /= True then t3d else
                    if is_solve(t3d3) == True then t3d3 else
                                solve_ t3d3 indexfh t3ds
                    where
                    f = convert_matrix3d_to_stroke_list t3d
                    e = foldl (flip delete) f listm
                    t3d3 =  if (length(e)/=0) then (add_cell_list t3d e) else t3d
                    len = (minimum (map (\x -> if length (x)<2 then 10 else length(x)) t3d))
                    indx = [i|i<-[0..nk-1]]
                    indexf = (filter(\ii -> length(t3d3!!ii)==len) indx)
                    indexfh = head (indexf)
                    t3ds = (t3d3!!indexfh)

mat = [[ 0, 0, 0, 0, 0, 0, 4, 0, 0], [ 3, 0, 6, 0, 0, 0, 0, 0, 0], [ 0, 0, 0, 1, 9, 6, 0, 3, 0], [ 0, 7, 0, 0, 0, 0, 0, 1, 0], [ 8, 0, 0, 2, 5, 0, 0, 9, 0], [ 0, 4, 0, 0, 0, 0, 8, 0, 0], [ 0, 6, 0, 4, 0, 9, 0, 0, 8], [ 0, 0, 5, 0, 0, 0, 0, 2, 0], [ 0, 0, 0, 5, 0, 0, 0, 0, 7]]::MatInt2d


main :: IO ()
main =  do
--mat<- intArray (n)
putStrLn (print_mat2d mat)
putStrLn ( print_mat2d(convert_matrix3d_to_matrix (solve (add_cell_list init_table (convert_matrix_to_stroke_list mat)) (convert_matrix_to_stroke_list mat) )))
