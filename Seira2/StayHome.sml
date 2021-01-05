open Char;
open String;
open List;
open Int;

type int_tup = int*int
val (Values_Array : int_tup array) = Array.array(3,(0,0))

(*https://stackoverflow.com/questions/3918288/turning-a-string-into-a-char-list-list-using-sml?fbclid=IwAR3OTwKwOssnWAODVeagorFiKh1OalQ01mZVMIMAvXj6Lc48Ga_emArd2XI*)
fun linelist file = 
    let val instr = TextIO.openIn file
        val str   = TextIO.inputAll instr
    in tokens isSpace str
       before
       TextIO.closeIn instr
    end

fun getminimap file   = 
    let 
        val L = map explode (linelist file)
        val minimap = Array2.fromList(L)
    in 
        minimap
    end


fun makeNewMap (map: char Array2.array) Airlist = 
    let 
        val ((N:int),(M:int)) = Array2.dimensions(map)
        val NewMap = Array2.array(N,M, 1000000)
        fun loop (i : int) (j : int) = 
            if j = M
            then (
                if (i = N-1)
                then ()
                else (
                    loop (i+1) 0
                )
            )
            else (
                if Array2.sub(map, i,j) = #"X"
                then(
                    Array2.update(NewMap, i, j, ~1);
                    loop i (j+1)
                )
                else (
                    if Array2.sub(map, i, j) = #"A"
                    then(
                        Array.update(Airlist, 0, ((i,j)::Array.sub(Airlist,0)));
                        Array2.update(NewMap, i,j , 999999);
                        loop i (j+1)                 
                    )
                    else (
                        if Array2.sub(map,i,j) = #"W"
                        then (
                            Array.update(Values_Array,0,(i,j));
                            Array2.update(NewMap, i, j, 0);
                            loop i (j+1)  

                        )
                        else(
                            if Array2.sub(map,i,j) = #"T"
                            then (
                                Array.update(Values_Array,1,(i,j));
                                loop i (j+1)  
                            )
                            else(
                                if Array2.sub(map,i,j) = #"S"
                                then (
                                    Array.update(Values_Array,2,(i,j));
                                    loop i (j+1)  
                                )
                                else(
                                    loop i (j+1)  
                                )
                            )
                        )
                    )
                )
            )
    in 
        loop 0 0;
        NewMap
    end    

                


fun flood_fill (matrix : int Array2.array) Airlist= 

    let
        val (N,M) = Array2.dimensions(matrix)
        val Q = Queue.mkQueue()
        val (x1,y1) = Array.sub(Values_Array, 0)
        fun loop a= 
            if (Queue.isEmpty(Q))
            then ()
            else(
                let 
                    val (t,(i,j)) = Queue.dequeue(Q)
                    val t2 = t + 2
                in 
                    if (i <0 orelse j <0 orelse i >= N orelse j >=M)
                    then (loop 1)
                    else(
                        if (Array2.sub(matrix, i,j) = ~1)
                        then (loop 1)
                        else(
                            if Array2.sub(matrix,i,j) = 999999
                            then(
                                let
                                    val List1 = Array.sub(Airlist,0)
                                    fun loop1 [] = ()
                                    | loop1 (H::T) = (Queue.enqueue(Q, (t+5,H));loop1 T)
                                in 
                                    Array2.update(matrix,i,j, t2);
                                    Queue.enqueue(Q, (t2, (i+1,j)));
                                    Queue.enqueue(Q, (t2, (i-1,j)));
                                    Queue.enqueue(Q, (t2, (i,j+1)));
                                    Queue.enqueue(Q, (t2, (i,j-1)));
                                    loop1 List1;
                                    loop 1
                                end    
                            )
                            else(
                                if Array2.sub(matrix, i,j) > t2
                                then(
                                    Array2.update(matrix,i,j,t2);
                                    Queue.enqueue(Q, (t2, (i+1,j)));
                                    Queue.enqueue(Q, (t2, (i-1,j)));
                                    Queue.enqueue(Q, (t2, (i,j+1)));
                                    Queue.enqueue(Q, (t2, (i,j-1)));
                                    loop 1
                                )
                                else(loop 1)
                            )
                            
                        )
                    ) 
                end       

            )
    in
        Queue.enqueue(Q, (0,(x1+1,y1)));
        Queue.enqueue(Q, (0,(x1-1,y1)));
        Queue.enqueue(Q, (0,(x1,y1-1)));
        Queue.enqueue(Q, (0,(x1,y1+1)));
        loop 1
    end        

fun make_answer ListAr  = 
    let
        val L = Array.sub(ListAr,0)
        val N = length L
        fun print_list [] = ()
            | print_list (x::xs) = ( print(x); print_list(xs))
    in
        if N = 0
        then (
            print("IMPOSSIBLE\n")
        )
        else(
            print(Int.toString(N));
            print("\n");
            print_list L;
            print("\n")
        )
    end    

fun find_path (matrix : int Array2.array) = 
    let
        val Q = Queue.mkQueue ()
        val (N,M) = Array2.dimensions(matrix)
        val (sot_i,sot_j) = Array.sub(Values_Array, 2)
        val (home_i, home_j) = Array.sub(Values_Array,1)
        val is_visited = Array2.array(N,M,false)
        val came_from = Array2.array(N,M, (~1,~1))
        val (answer : string list array) = Array.array(1,[])
    in
        Queue.enqueue(Q, (0,(sot_i,sot_j)));
        while (not (Queue.isEmpty(Q))) do (
            let 
                val (t,(i,j)) = Queue.dequeue(Q)
            in 
                if (i<0 orelse j <0 orelse i>=N orelse j >=M) then()
                else (
                    if Array2.sub(matrix,i,j) = (~1) then ()
                    else (
                        if ( i = home_i andalso j = home_j)
                        then (
                            let
                                fun while1 x y =(
                                  
                                    if (x=sot_i andalso y = sot_j) then ()
                                    else(
                                        let
                                            val prev_x = x
                                            val prev_y = y
                                            val (curr_x,curr_y) = Array2.sub(came_from, prev_x, prev_y)
                                        in
                                            if (curr_x = prev_x +1 andalso curr_y = prev_y)
                                            then(
                                                Array.update(answer, 0, "U"::Array.sub(answer,0));
                                                while1 curr_x curr_y
                                            )
                                            else(
                                                if (curr_x = prev_x -1 andalso curr_y = prev_y)
                                                then(
                                                    Array.update(answer, 0, "D"::Array.sub(answer,0));
                                                    while1 curr_x curr_y
                                                )
                                                else(
                                                    if (curr_x = prev_x  andalso curr_y = prev_y+1)
                                                    then(
                                                        Array.update(answer, 0, "L"::Array.sub(answer,0));
                                                        while1 curr_x curr_y
                                                    )
                                                    else(
                                                        if (curr_x = prev_x  andalso curr_y = prev_y -1)
                                                        then(
                                                            Array.update(answer, 0, "R"::Array.sub(answer,0));
                                                            while1 curr_x curr_y
                                                        )
                                                        else()
                                                    )
                                                ) 
                                            )
                                        end               
                                    )
                                )
                            in
                                while1 home_i home_j
                            end             
                        )
                        else (
                            let
                                val neighbors = [(i+1,j), (i,j-1), (i,j+1), (i-1,j)] 
                                fun new_loop [] = ()
                                    | new_loop (x::xs) = 
                                        let
                                            val (new_i,new_j) = x
                                        in 
                                            if (new_i>=0 andalso new_j >=0 andalso new_i <N andalso new_j <M)
                                            then(
                                                if (Array2.sub(is_visited,new_i,new_j) = false andalso Array2.sub(matrix,new_i,new_j) > t+1)
                                                then (
                                                    Array2.update(is_visited,new_i, new_j, true);
                                                    Array2.update(came_from, new_i, new_j, (i,j));
                                                    Queue.enqueue(Q, (t+1, (new_i, new_j)));
                                                    new_loop xs
                                                )
                                                else (new_loop xs)
                                            ) 
                                            else (new_loop xs)
                                        end
                            in
                                new_loop neighbors
                            end                    
                        )
                    )
                )
            end     
        );
        make_answer answer 
    end       




fun stayhome file =
    let 
        val (Airlist: int_tup list array) = Array.array(1,[])
        val x = getminimap file;
        val matrix = makeNewMap x Airlist;
    in 
        flood_fill matrix Airlist;
        find_path matrix
    end        