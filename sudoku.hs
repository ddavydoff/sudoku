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

n = 9
nm = 3  
nk = 81 

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
show_2d_ [] = ""
show_2d_ (x:xs) = addSpaces (n-length(dds)) ++ dds ++ " " ++ show_2d_ (xs) where dds = show x
              
print_mat2d :: [[Int]] -> String
print_mat2d [] = ""
print_mat2d (x:xs) = (show_2d_ x) ++ "\n" ++ (print_mat2d xs)
  
show_d :: ListMInt -> String
show_d [] = ""
show_d (x:xs) = show x ++ show_d(xs)

show_2d :: MatInt2d -> String
show_2d [] = ""
show_2d (x:xs) = addSpaces (n-length(dds)) ++ dds ++ " " ++ show_2d (xs) where dds = show_d (x)

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

conv_p2l = \x -> \y -> x*n+y
conv_l2p = \x -> (x `div` n,x `mod` n)
fx = \x -> \p -> x+3*((get_quad_l p) `div` nm)
fy = \x -> \p -> x+3*((get_quad_l p) `mod` nm)
get_quad = \(x,y) -> (x `div` nm)*nm + y `div` nm
get_quad_l = \p -> (\(x,y) -> (x `div` nm)*nm + y `div` nm )(conv_l2p p)

---------------------------------------------------
-- Function opeartion with matrix

init_table = replicate nk [1,2..9]

gen_stroke_list_wl:: Int -> Int -> [Int]
gen_stroke_list_wl 1 p =  [ i*n+(p `mod` n)| i<-[0..(n-1)],i/=(p `div` n) ]
gen_stroke_list_wl 2 p =  [ (p `div` n)*n+i| i<-[0..(n-1)],i/=(p `mod` n) ]
gen_stroke_list_wl 3 p =  [ conv_p2l (fx i p) (fy j p) | i<-[0..(nm-1)],j<-[0..(nm-1)],((fx i p)/=(p `div` n))||((fy j p)/=(p `mod` n)) ]

delete_from_cell__::MatInt2d->(Int,Int)->[[Int]]
delete_from_cell__ t3d (zz,zp) = [if i==zp then (delete zz (t3d!!i)) else (t3d!!i) | i<-[0..nk-1] ]

delete_from_cell_::MatInt2d->[(Int,Int)]->[[Int]]
delete_from_cell_ t3d [] = t3d
delete_from_cell_ t3d (x:xs) = delete_from_cell_ (delete_from_cell__ t3d x) xs

set_to_cell__::MatInt2d->(Int,Int)->[[Int]]
set_to_cell__ t3d (zz,zp) = [if i==zp then [zz] else (t3d!!i)|i<-[0..nk-1] ]

add_cell::MatInt2d->(Int,Int)->[[Int]]
add_cell t3d (zp,zz) = delete_from_cell_ t3d1 (map   (\x -> (zz,x)) listxyz)
                    where t3d1=set_to_cell__ t3d (zz,zp) 
                          listxyz= (gen_stroke_list_wl 1 zp)++(gen_stroke_list_wl 2 zp )++(gen_stroke_list_wl 3 zp )

add_cell_list::[[Int]]->[(Int,Int)]->[[Int]]
add_cell_list t3d [] = t3d
add_cell_list t3d (x:xs) = add_cell_list (add_cell t3d x) xs

is_correct::MatInt2d->Bool
is_correct []=True
is_correct (x:xs)=(length(x)/=0)&&is_correct(xs)

is_solve::MatInt2d->Bool
is_solve []=True
is_solve (x:xs)=(length(x)==1)&&is_solve(xs)

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


main :: IO ()
main =  do
--mat<- intArray (n)
let mat = [[ 0, 0, 0, 0, 0, 0, 4, 0, 0], [ 3, 0, 6, 0, 0, 0, 0, 0, 0], [ 0, 0, 0, 1, 9, 6, 0, 3, 0], [ 0, 7, 0, 0, 0, 0, 0, 1, 0], [ 8, 0, 0, 2, 5, 0, 0, 9, 0], [ 0, 4, 0, 0, 0, 0, 8, 0, 0], [ 0, 6, 0, 4, 0, 9, 0, 0, 8], [ 0, 0, 5, 0, 0, 0, 0, 2, 0], [ 0, 0, 0, 5, 0, 0, 0, 0, 7]]
let list_matrix2=convert_matrix_to_stroke_list mat
putStrLn (print_mat2d mat)
putStrLn ( print_mat3d (solve (add_cell_list init_table list_matrix2) list_matrix2 ))
