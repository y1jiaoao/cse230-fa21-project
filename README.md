# cse230-fa21-project

Team Members (Name, Student ID, UCSD Email, Github Username):  
<ul>
  <li>Guanghao Li, A59002482, gul012@ucsd.edu, y1jiaoao</li>
  <li>Miaoran Chen, A59001732, mic005@ucsd.edu meroyucsd</li>
  <li>Wending Peng, A59003175, w4peng@ucsd.edu, wendingp</li>
  <li>Miao Hao, A59004584, mhao@ucsd.edu, harry11162</li>
</ul>

<br/>

Proposal: A Gomoku (Five in a Row) game  


Description: Gomoku (also known as Five In A Row, Gobang, Connect 5) is a purely strategic chess game where two people play against each other. Two sides use black and white chess pieces respectively, and place them on the intersection of vertical and horizontal lines of the chessboard. The side that first forms a horizontal, vertical, or diagonal line of five pieces wins. In our project, we decide to implement a Gomoku game by haskell. Our Gomoku game contains two kinds of mode. The first is player vs AI locally. The second is player vs player via network.

<br/>
Architecture: 
<ul>
  <li>Interface: choose the mode you want to play</li>
  <ul>
    <li>Mode 1: player vs AI</li>
    <ul>
      <li>1. directly form the board</li>
      <li>2. return the result when either side reaches 5 or there is no place to go (i.e. draw)</li>
    </ul>
    <li>Mode 2: player vs player</li>
    <ul>
      <li>1. choose server or client you are</li>
      <li>2. wait until the connection is built and the board is formed</li>
      <li>3. return the result when either side reaches 5 or there is no place to go (i.e. draw)</li>
    </ul>
  </ul>
</ul>

Challenges: It may take time to learn how to deal with the network part.  
(Anyway, we expect to meet our goals until the deadline.)
