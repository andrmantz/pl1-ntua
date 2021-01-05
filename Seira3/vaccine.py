import queue
import sys
from collections import deque

stack1= deque()
stack2=deque()
mydic = { 'A' : 0, 'U' : 1, 'C':2, 'G':3}


def read():
    with open (sys.argv[1], 'r') as f:
        t = int(f.readline())
        times_counter =0
        for line in f:
            times_counter+=1
            if times_counter==t+1:
                break
            stack1.clear()
            stack2.clear()
            for char in line:
                if (char=='\n'):
                    break
                stack1.append(char)
            if len(stack1)==1:
                print('p')
            elif len(stack2)==2:
                print('pp')
            else:
                bfs(stack1, stack2)
        f.close()
            
#(stack1, stack2, move,flag)
def bfs(s1, s2):
    flag = [0,0,0,0] #[flagA,flagU,flagC,flagG]
    remaining = queue.Queue(maxsize = sys.maxsize)
    seen = set()
    parent = {} #dictionary gia backtrack
    Answer = []
    x = s1.pop()
    flag[mydic[x]] = 2
    s2.append(x) # i proti kinisi einai panta p
    x1 = tuple(s1)
    x2 = tuple(s2)
    initial= ( x1, x2, 'p')
    seen.add(initial)
    remaining.put((s1, s2, 'p', flag))
    while (not remaining.empty()):
        (list1,list2,x, flags) = remaining.get()
        l1 = tuple(list1)
        l2 = tuple(list2)
        currState = (l1, l2, x)

        if (len(list1)==1 and flags[mydic[list1[0]]]!=1):
            Answer.append('p')

            while (True):                    
                (x,y,z) = currState
                Answer.append(z)
                if (currState==initial):
                    break
                currState = parent[currState]

            Answer.reverse()
            print("".join(Answer))
            return

        if (list1[-1]==list2[-1]):
                push1 = list1.copy()
                push2 = list2.copy()
                push2.append(push1.pop())
                x1 = tuple(push1)
                x2 = tuple(push2)
                if ((x1,x2,'p')) not in seen:
                    remaining.put((push1,push2,'p', flags))
                    seen.add((x1,x2,'p'))
                    parent[(x1,x2,'p')] = currState
        
        else:
            #c
            if (x=='p'):
                new1 = list1.copy()
                for i in range(len(new1)):
                    if (new1[i]=='A'):
                        new1[i]='U'
                    elif (new1[i]=='U'):
                        new1[i]='A'
                    elif (new1[i]=='G'):
                        new1[i]='C'
                    else:
                        new1[i]='G'
                x1= tuple(new1)
                if (x1,x2,'c') not in seen:
                    remaining.put((new1, list2, 'c', flags))
                    seen.add((x1,l2,'c'))
                    parent[(x1,l2,'c')] = currState

            #p
            if (flags[mydic[list1[-1]]]!=1):
                push1 = list1.copy()
                push2 = list2.copy()
                push2.append(push1.pop())
                x1 = tuple(push1)
                x2 = tuple(push2)
                if ((x1,x2,'p') not in seen):
                    f = flags.copy()
                    f[mydic[push2[-1]]]=2
                    f[mydic[push2[-2]]]=1
                    remaining.put((push1,push2,'p', f))
                    seen.add((x1,x2,'p'))
                    parent[(x1,x2,'p')] = currState
            
            #r
            if (x=='p' or x=='c') and (len(list2)!=1 and not (flags[mydic[list1[-1]]]==1 and list1[-1]!=list2[0])):

                rev = list2.copy()
                rev.reverse()
                x2=tuple(rev)
                if (l1, x2, 'r') not in seen:
                    f2=flags.copy()
                    f2[mydic[rev[0]]]=1
                    f2[mydic[rev[-1]]]=2
                    remaining.put((list1,rev,'r',f2))
                    seen.add((l1,x2,'r'))
                    parent[(l1,x2,'r')] = currState

            
                    

read()