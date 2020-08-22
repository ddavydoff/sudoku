import Data.List

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
    
-------------------------------------------
-- Conversion function
convert_matrix_to_pair_list :: [[Int]] -> [(Int,Int,Int)]
convert_matrix_to_pair_list x 
        = map (\(i,j)->(i,j,(x !! i) !! j)) sort
         where indx  = [(i,j) | i<-[0..n-1],j<-[0..n-1]]
               sort     = filter (\ (ii,jj) ->  0 /= (x !! ii) !! jj) indx

convert_matrix_to_stroke_list :: [[Int]] -> [(Int,Int)]
convert_matrix_to_stroke_list x 
        = map (\(i,j)->(i*n+j,(x !! i) !! j)) sort
         where indx  = [(i,j) | i<-[0..n-1],j<-[0..n-1]]
               sort  = filter (\ (ii,jj) ->  0 /= (x !! ii) !! jj) indx

convert_matrix3d_to_stroke_list :: [[Int]] -> [(Int,Int)]
convert_matrix3d_to_stroke_list x 
        = map (\i->(i,(x !! i) !! 0)) sort
         where indx  = [i | i<-[0..nk-1]]
               sort  = filter (\ ii ->  length(x !! ii)==1) indx

convert_listn_pair_to_listn = \x -> \y -> x*n+y
convert_listn_to_pair = \x -> (x `div` n,x `mod` n)
fx = \x -> \p -> x+3*((get_quad_l p) `div` nm)
fy = \x -> \p -> x+3*((get_quad_l p) `mod` nm)

---------------------------------------------------
-- Main function

gen_stroke_list:: Int -> Int-> [Int]
-- column
gen_stroke_list 1 p =  [ i*n+p| i<-[0..(n-1)] ]
-- strokes
gen_stroke_list 2 p =  [ p*n+i| i<-[0..(n-1)] ]
-- quadrant
gen_stroke_list 3 p =  [ convert_listn_pair_to_listn (fx i p) (fy j p) | i<-[0..(nm-1)],j<-[0..(nm-1)] ]

gen_stroke_list_w:: Int -> Int -> (Int,Int)-> [Int]
-- column
gen_stroke_list_w 1 p (x,y) =  [ i*n+p| i<-[0..(n-1)],i/=x ]
-- strokes
gen_stroke_list_w 2 p (x,y) =  [ p*n+i| i<-[0..(n-1)],i/=y ]
-- quadrant
gen_stroke_list_w 3 p (x,y) =  [ convert_listn_pair_to_listn (fx i p) (fy j p) | i<-[0..(nm-1)],j<-[0..(nm-1)],((fx i p)/=x)||((fy j p)/=y) ]

gen_stroke_list_wl:: Int -> Int -> [Int]
-- column
gen_stroke_list_wl 1 p =  [ i*n+(p `mod` n)| i<-[0..(n-1)],i/=(p `div` n) ]
-- strokes
gen_stroke_list_wl 2 p =  [ (p `div` n)*n+i| i<-[0..(n-1)],i/=(p `mod` n) ]
-- quadrant
gen_stroke_list_wl 3 p =  [ convert_listn_pair_to_listn (fx i p) (fy j p) | i<-[0..(nm-1)],j<-[0..(nm-1)],((fx i p)/=(p `div` n))||((fy j p)/=(p `mod` n)) ]
                            

delete_from_list::[Int]->Int->[Int]
delete_from_list t2d zz = filter (\x -> x/=zz) t2d

delete_from_cell__::[[Int]]->(Int,Int)->[[Int]]
delete_from_cell__ t3d (zz,zp) = [if i==zp then (delete_from_list (t3d!!i) zz) else (t3d!!i) | i<-[0..(nk-1)] ]

delete_from_cell_::[[Int]]->[(Int,Int)]->[[Int]]
delete_from_cell_ t3d [] = t3d
delete_from_cell_ t3d (x:xs) = delete_from_cell_ (delete_from_cell__ t3d x) xs

set_to_cell__::[[Int]]->(Int,Int)->[[Int]]
set_to_cell__ t3d (zz,zp) = [if i==zp then [zz] else (t3d!!i)|i<-[0..(nk-1)] ]

get_quad :: (Int,Int) -> Int
get_quad (x,y) = (x `div` nm)*nm + y `div` nm

get_quad_l :: Int -> Int
get_quad_l p = (x `div` nm)*nm + y `div` nm
                where 
                    (x,y)=convert_listn_to_pair p

add_cell::[[Int]]->(Int,Int)->[[Int]]
add_cell t3d (zp,zz) = delete_from_cell_ t3d1 (map   (\x -> (zz,x)) listxyz)
                    where t3d1=set_to_cell__ t3d (zz,zp) 
                          listxyz= (gen_stroke_list_wl 1 zp)++(gen_stroke_list_wl 2 zp )++(gen_stroke_list_wl 3 zp )

add_cell_list::[[Int]]->[(Int,Int)]->[[Int]]
add_cell_list t3d [] = t3d
add_cell_list t3d (x:xs) = add_cell_list (add_cell t3d x) xs

init_table ::  [[Int]]
init_table = replicate nk [1,2..9]


main :: IO ()
main =  do
--mat<- intArray (n)
let mat = [[ 0, 0, 0, 0, 0, 0, 4, 0, 0], [ 3, 0, 6, 0, 0, 0, 0, 0, 0], [ 0, 0, 0, 1, 9, 6, 0, 3, 0], [ 0, 7, 0, 0, 0, 0, 0, 1, 0], [ 8, 0, 0, 2, 5, 0, 0, 9, 0], [ 0, 4, 0, 0, 0, 0, 8, 0, 0], [ 0, 6, 0, 4, 0, 9, 0, 0, 8], [ 0, 0, 5, 0, 0, 0, 0, 2, 0], [ 0, 0, 0, 5, 0, 0, 0, 0, 7]]
print (take 1 mat)
print (take 1 (drop 1 mat))
print (take 1 (drop 2 mat))
print (take 1 (drop 3 mat))
print (take 1 (drop 4 mat))
let list_matrix = convert_matrix_to_pair_list mat
--print list_matrix
--let gen_list=gen_stroke_list 3 3
--print gen_list
let list_matrix2 = convert_matrix_to_stroke_list mat
print list_matrix2
--let gen_list2=gen_stroke_list_w 3 3 (3,2)
--print gen_list2
let table3d = init_table
--print table3d
--let table3d1 = add_cell table3d (1,2)
let table3d2 = add_cell_list table3d list_matrix2
print (take 9 table3d2)
print (take 9 (drop 9 table3d2))
print (take 9 (drop 18 table3d2))
print (take 9 (drop 27 table3d2))
print (take 9 (drop 36 table3d2))
print (take 9 (drop 45 table3d2))
print (take 9 (drop 54 table3d2))
print (take 9 (drop 63 table3d2))
print (take 9 (drop 72 table3d2))
--let lst=gen_stroke_list_wl 3 22
--let lst2=map   (\x -> (x,9)) lst
--print lst2
let f=convert_matrix3d_to_stroke_list table3d2
print f
let table3d3 = add_cell_list table3d2 (foldl (flip delete) f list_matrix2)
print (take 9 table3d3)
print (take 9 (drop 9 table3d3))
print (take 9 (drop 18 table3d3))
print (take 9 (drop 27 table3d3))
print (take 9 (drop 36 table3d3))
print (take 9 (drop 45 table3d3))
print (take 9 (drop 54 table3d3))
print (take 9 (drop 63 table3d3))
print (take 9 (drop 72 table3d3))
let g=convert_matrix3d_to_stroke_list table3d3
print g
