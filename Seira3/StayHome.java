import java.io.*;
import java.util.*;



class block{
    private int x;
    private int y;
    private int t;
    private block previous;
    public static int max_x;
    public static int max_y;

    public block(int x, int y, int t){
        this.x = x;
        this.y = y;
        this.t = t;
        this.previous=null;
    }

    public block(int x, int y, int t, block previous){
        this.x = x;
        this.y = y;
        this.t = t;
        this.previous=previous;
    }


 /*   public Collection<block> sot_next(){
        Collection<block> states = new ArrayList<>();

        if (x+1 < globs.max_x) states.add(new block(x+1, y, t+1, this ));
        if (y-1>=0) states.add(new block(x, y-1, t+1, this ));
        if (y+1 < globs.max_y) states.add(new block(x, y+1, t+1, this ));
        if (x-1 >=0) states.add(new block(x-1, y, t+1, this ));

        return states;
    }*/

    public Collection<block> virus_next(){
        Collection<block> states = new ArrayList<>();
        

        if (x+1<max_x) states.add(new block(x+1, y, t+2));
        if (y-1>=0) states.add(new block(x, y-1, t+2));
        if (y+1<max_y) states.add(new block(x, y+1, t+2));
        if (x-1>=0) states.add(new block(x-1, y, t+2));

        return states;
    }

    public int get_x(){
        return x;
    }
    public int get_y(){
        return y;
    }
    public int get_t(){
        return t;
    }

    public block get_prev(){
        return previous;
    }
    
    @Override
    public String toString() {
      StringBuilder sb = new StringBuilder("Block: ");
      sb.append("x=").append(x);
      sb.append(", y=").append(y);
      sb.append(", t=").append(t);
      sb.append(", prev=").append(previous);
      return sb.toString();
    }

    
}


public class StayHome{
    public static void main( String[] args) {
        File infile = new File(args[0]);
        
        List<Integer> airlist = new ArrayList<Integer>();
        int x=0,y=0;
        int t=0;
        int i=0, j=0;
        int[][] map;
        char ch;
        Queue<block> my_bfs_q = new ArrayDeque<>();
        block temp;
        int x1=0, x2=0;



        int x_v=0, y_v=0, x_s=0, y_s=0, x_t=0, y_t=0;
        try{//diabasma arxeiou/xarti
            BufferedReader b1 = new BufferedReader(new FileReader(infile));
            String line;
            while ((line=b1.readLine())!=null){ //diabazo olo to arxeio gia na bro tis diastaseis tou grid
                x++;
                y = line.length();
            } 
            map = new int[x][y];

            BufferedReader br = new BufferedReader(new FileReader(infile));
            
            for (i=0;i<x; i++){
                line = br.readLine();
                for (j=0; j<line.length(); j++){
                    ch = line.charAt(j);
                    if (ch=='X'){
                        map[i][j] = -1;
                    }
                    else if (ch=='A'){
                        map[i][j] = 1073741820;
                        airlist.add(i);
                        airlist.add(j);
                    }
                    else if (ch=='W'){
                        map[i][j] = 0;
                        x_v=i;
                        y_v=j;
                    }
                    else{
                        if (ch=='S'){
                            x_s=i;
                            y_s=j;
                        }
                        else if (ch=='T'){
                            x_t=i;
                            y_t=j;
                        }
                        map[i][j] = 1073741822;
                    }
                }
            }//diabasma telos
            block.max_x = x;
            block.max_y = y;
            boolean[][] visited = new boolean[x][y];
            for (i=0; i<x; i++){
                for (j=0;j<y;j++) visited[i][j]=false;
            }
            //BFS gia io
            block virus_initial = new block(x_v, y_v, -2);
            for (block n: virus_initial.virus_next()){
                my_bfs_q.add(n);
            }

            while (!my_bfs_q.isEmpty()){
                temp = my_bfs_q.remove();
                i = temp.get_x();
                j = temp.get_y();
                t = temp.get_t();
                if (i<0 || i >=x || j<0 || j>=y ){
                    continue;
                }
                if (map[i][j] == -1){
                    continue;
                }


                if (map[i][j] ==1073741820 ){
                    map[i][j] =t+2;
                    for (block n:temp.virus_next()){
                        my_bfs_q.add(n);
                    }
                    for (int b=0; b<airlist.size();b=b+2){
                        x1= airlist.get(b);
                        x2=airlist.get(b+1);
                        if (map[x1][x2]> t+7){
                            map[x1][x2]=t+7;
                            block airport = new block(x1,x2, t+5);
                            for (block k: airport.virus_next()){
                                my_bfs_q.add(k);
                            }
                        }
                    }    
                }
                else if (map[i][j]>t+2){
                    map[i][j] = t+2;
                    for (block n: temp.virus_next()){
                        my_bfs_q.add(n);
                    }
                }
            }
         /*   for (int kk =0; kk<x; kk++){
                for (int yy=0; yy<y; yy++){
                    System.out.print(map[kk][yy] +" ");
                }
                System.out.println();
            }*/
            t=0;
            block sotos = new block(x_s, y_s, 0);
            my_bfs_q.add(sotos);
            visited[x_s][y_s] = true;
            while(!my_bfs_q.isEmpty()){
                temp = my_bfs_q.remove();
                i= temp.get_x();
                j= temp.get_y();
                t= temp.get_t();

                if (i==x_t && j==y_t){
                    List<Character> answer= new ArrayList<>();
                  //  System.out.print(x_s + " "); System.out.println(y_s);
                    while (temp.get_x()!=x_s || temp.get_y()!=y_s){
                  //      System.out.print(temp.get_x()+" "); System.out.println(temp.get_y());
                 //       System.out.print(temp.get_prev().get_x()+" "); System.out.println(temp.get_prev().get_y());

                        if (temp.get_x()==temp.get_prev().get_x()+1){
                            answer.add('D');
                        }
                        else if (temp.get_x()==temp.get_prev().get_x()-1){
                            answer.add('U');
                        }
                        else if (temp.get_y()==temp.get_prev().get_y()+1){
                            answer.add('R');
                        }
                        else if (temp.get_y()==temp.get_prev().get_y()-1){
                            answer.add('L');
                        }
                        temp=temp.get_prev();
                    }
                    
                    System.out.println(answer.size());
                    for(i=answer.size()-1;i>-1; i--){
                        System.out.print(answer.get(i));
                    }
                    System.out.println();
                    return;
                    

                }

                if ((i+1<x) && (map[i+1][j]>=0 && visited[i+1][j]==false && map[i+1][j] >t+1)){
                    visited[i+1][j] = true;
                    my_bfs_q.add(new block(i+1, j, t+1, temp));
                }  
                if ((j-1>=0) && (map[i][j-1]>=0 && visited[i][j-1]==false && map[i][j-1] >t+1)){
                    visited[i][j-1] = true;
                    my_bfs_q.add(new block(i, j-1, t+1, temp));
                }  
                if ((j+1<y) && (map[i][j+1]>=0 && visited[i][j+1]==false && map[i][j+1] >t+1)){
                    visited[i][j+1] = true;
                    my_bfs_q.add(new block(i, j+1, t+1, temp));
                }  
                if ((i-1>=0) && (map[i-1][j]>=0 && visited[i-1][j]==false && map[i-1][j] >t+1)){
                    visited[i-1][j] = true;
                    my_bfs_q.add(new block(i-1, j, t+1, temp));
                }  

                /*for (block n: temp.sot_next()){
                    x1=n.get_x();x2= n.get_y();

                    if ((x1<0 || x1>=x || x2<0 || x2>=y) || visited[x1][x2]==true){
                            continue;
                    }
                    if ((map[x1][x2]<0) || (map[x1][x2] <=t+1)){
                        continue;
                    }    
                    visited[x1][x2] = true;
                    my_bfs_q.add(n);
                }*/

            }

            System.out.println("IMPOSSIBLE");
            return;
        }
        catch (Exception e){}     
    }
}