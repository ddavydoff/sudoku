import Data.List

n = 9
nm = 3
nk = 81

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
-- IO function

intArray :: Int -> IO [[Int]]
intArray 0 = return []
intArray x = do
    str <- getLine
    nextInt <- intArray (x - 1)
    let int = map read (words str)::[Int]
    return (int:nextInt)
    
print_mat2d :: MatInt2d -> String
print_mat2d [] = ""
print_mat2d x = (show (take 1 x)) ++ "\n" ++ (print_mat2d (drop 1 x))

addSpaces :: Int -> String
addSpaces 0 = ""
addSpaces n = " " ++ addSpaces (n-1) 
  
show_d :: ListMInt -> String
show_d [] = ""
show_d (x:xs) = show x ++ show_d(xs)

show_2d :: MatInt2d -> String
show_2d [] = ""
show_2d (x:xs) = addSpaces (n-length(dds)) ++ dds ++ " " ++ show_2d (xs)
              where 
              dds = show_d (x)

print_mat3d :: MatInt2d -> String
print_mat3d [] = ""
print_mat3d x = (show_2d  (take n x)) ++ "\n" ++ (print_mat3d (drop n x))   
    
-------------------------------------------
-- Conversion function
convert_matrix_to_pair_list :: MatInt2d -> [(Int,Int,Int)]
convert_matrix_to_pair_list x 
        = map (\(i,j)->(i,j,(x !! i) !! j)) sort
         where indx  = [(i,j) | i<-[0..n-1],j<-[0..n-1]]
               sort     = filter (\ (ii,jj) ->  0 /= (x !! ii) !! jj) indx

convert_matrix_to_stroke_list :: MatInt2d -> [(Int,Int)]
convert_matrix_to_stroke_list x 
        = map (\(i,j)->(i*n+j,(x !! i) !! j)) sort
         where indx  = [(i,j) | i<-[0..n-1],j<-[0..n-1]]
               sort  = filter (\ (ii,jj) ->  0 /= (x !! ii) !! jj) indx

convert_matrix3d_to_stroke_list :: MatInt2d -> [(Int,Int)]
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
-- Main function

gen_stroke_list:: Int -> Int-> [Int]
-- column
gen_stroke_list 1 p =  [ i*n+p| i<-[0..(n-1)] ]
-- strokes
gen_stroke_list 2 p =  [ p*n+i| i<-[0..(n-1)] ]
-- quadrant
gen_stroke_list 3 p =  [ conv_p2l (fx i p) (fy j p) | i<-[0..(nm-1)],j<-[0..(nm-1)] ]

gen_stroke_list_w:: Int -> Int -> (Int,Int)-> [Int]
-- column
gen_stroke_list_w 1 p (x,y) =  [ i*n+p| i<-[0..(n-1)],i/=x ]
-- strokes
gen_stroke_list_w 2 p (x,y) =  [ p*n+i| i<-[0..(n-1)],i/=y ]
-- quadrant
gen_stroke_list_w 3 p (x,y) =  [ conv_p2l (fx i p) (fy j p) | i<-[0..(nm-1)],j<-[0..(nm-1)],((fx i p)/=x)||((fy j p)/=y) ]

gen_stroke_list_wl:: Int -> Int -> [Int]
-- column
gen_stroke_list_wl 1 p =  [ i*n+(p `mod` n)| i<-[0..(n-1)],i/=(p `div` n) ]
-- strokes
gen_stroke_list_wl 2 p =  [ (p `div` n)*n+i| i<-[0..(n-1)],i/=(p `mod` n) ]
-- quadrant
gen_stroke_list_wl 3 p =  [ conv_p2l (fx i p) (fy j p) | i<-[0..(nm-1)],j<-[0..(nm-1)],((fx i p)/=(p `div` n))||((fy j p)/=(p `mod` n)) ]

union_2cell::[[Int]]->Int->Int->[Int]
union_2cell t3d x1 x2 = []

union_cell::[[Int]]->[Int]->[Int]
union_cell t3d (x:xs) = []
                            
delete_from_list::[Int]->Int->[Int]
delete_from_list t2d zz = filter (\x -> x/=zz) t2d

delete_from_cell__::[[Int]]->(Int,Int)->[[Int]]
delete_from_cell__ t3d (zz,zp) = [if i==zp then (delete_from_list (t3d!!i) zz) else (t3d!!i) | i<-[0..(nk-1)] ]

delete_from_cell_::[[Int]]->[(Int,Int)]->[[Int]]
delete_from_cell_ t3d [] = t3d
delete_from_cell_ t3d (x:xs) = delete_from_cell_ (delete_from_cell__ t3d x) xs

set_to_cell__::[[Int]]->(Int,Int)->[[Int]]
set_to_cell__ t3d (zz,zp) = [if i==zp then [zz] else (t3d!!i)|i<-[0..(nk-1)] ]

add_cell::[[Int]]->(Int,Int)->[[Int]]
add_cell t3d (zp,zz) = delete_from_cell_ t3d1 (map   (\x -> (zz,x)) listxyz)
                    where t3d1=set_to_cell__ t3d (zz,zp) 
                          listxyz= (gen_stroke_list_wl 1 zp)++(gen_stroke_list_wl 2 zp )++(gen_stroke_list_wl 3 zp )

add_cell_list::[[Int]]->[(Int,Int)]->[[Int]]
add_cell_list t3d [] = t3d
add_cell_list t3d (x:xs) = add_cell_list (add_cell t3d x) xs

is_correct::[[Int]]->Bool
is_correct []=True
is_correct (x:xs)=(length(x)/=0)&&is_correct(xs)

init_table ::  [[Int]]
init_table = replicate nk [1,2..9]

main :: IO ()
main =  do
--mat<- intArray (n)
let mat = [[ 0, 0, 0, 0, 0, 0, 4, 0, 0], [ 3, 0, 6, 0, 0, 0, 0, 0, 0], [ 0, 0, 0, 1, 9, 6, 0, 3, 0], [ 0, 7, 0, 0, 0, 0, 0, 1, 0], [ 8, 0, 0, 2, 5, 0, 0, 9, 0], [ 0, 4, 0, 0, 0, 0, 8, 0, 0], [ 0, 6, 0, 4, 0, 9, 0, 0, 8], [ 0, 0, 5, 0, 0, 0, 0, 2, 0], [ 0, 0, 0, 5, 0, 0, 0, 0, 7]]
putStrLn (print_mat2d mat)
let list_matrix = convert_matrix_to_pair_list mat
let list_matrix2 = convert_matrix_to_stroke_list mat
--putStrLn ( print_mat2d list_matrix2 )
-------------------------------------------------------------
let table3d = init_table
let table3d2 = add_cell_list table3d list_matrix2
putStrLn ( print_mat3d table3d2 )
let f=convert_matrix3d_to_stroke_list table3d2
print f
let e =foldl (flip delete) f list_matrix2
print e
let table3d3 = add_cell_list table3d2 e
putStrLn ( print_mat3d table3d3 )
-------------------------------------------------------------
let g=convert_matrix3d_to_stroke_list table3d3
print g
