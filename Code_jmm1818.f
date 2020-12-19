
{ ----------------------------------- CONWAY'S LIFE ------------------------------------ }
{                                                                                        }
{ This code describes the process to obtain data following Conway's life's rules.        }
{                                                                                        }
{ It also transforms the data into a .bmp image in a different window from the console   }
{ and contains the words that describe how to save the data and the .bmp images.         }



{ ------------------------------------- Variables -------------------------------------- }

variable grid               { Dimension of the side of the grid                          }
variable value_x            { x values of the matrix -- coordinates matrix               }
variable value_y            { x values of the matrix -- coordinates matrix               }
variable counts             { Stores counts of cells alive around a certain cell         } 
variable bmp-x-size         { x dimension of bmp file                                    }
variable bmp-y-size         { y dimension of bmp file                                    }
variable bmp-size           { Total number of bmp elements = (x * y)                     }
variable bmp-address        { Stores start address of bmp file # 1                       }
variable bmp-length         { Total number of chars in bmp including header block        }
variable bmp-x-start        { Initial x position of upper left corner                    }
variable bmp-y-start        { Initial y position of upper left corner                    }
variable bmp-window-handle  { Variable to store the handle used to ID display window     }
variable test-file-id       { Create Variable to hold file id handle                     }



{ --------------------------------------- Random --------------------------------------- }

CREATE SEED  123456789 ,

: Rnd ( n -- rnd )      { Returns single random number less than n                       }
   SEED                 { Minimal version of SwiftForth Rnd.f                            }
   DUP >R               { Algorithm Rick VanNorman  rvn@forth.com                        }
   @ 127773 /MOD 
   2836 * SWAP 16807 * 
   2DUP > IF - 
   ELSE - 2147483647 +  
   THEN  DUP R> !
   SWAP MOD ;



{ ---------------------------------------- Grid ---------------------------------------- }

100 grid !
: grid_do 
    grid @ grid @ * ;
grid_do allocate drop constant array_xy       { Array to allocate alive and dead cells   }
grid_do allocate drop constant array_counts   { Array to allocate the number of counts   }

: reset_arrays                                { Resets the values in the arrays          }
    array_xy grid_do 0 fill
    array_counts grid_do 0 fill ;
: reset_counts                                { Resets the values of the counts          }             
    0 counts c! ;

: random_generation                           { Generates 0 and 1 random values for each } 
    grid_do 0 do 2 rnd array_xy I + c! loop ; { position of the array_xy                 }



{ -------------------------------------- Counting -------------------------------------- }

: values_x_y                                  { Stores the values of x and y coordinates }
    value_x c! value_y c! ;

: array_xy_@                                  { Calls a point in the array_xy            }
    array_xy rot grid @ * rot + + c@ ;
: array_counts_@                              { Calls a point in the array_counts        }
    array_counts rot grid @ * rot + + c@ ;
: array_xy_!                                  { Introduces a value in a certain position }
    array_xy rot grid @ * rot + + c! ;        { of array_xy                              }
: array_counts_!                              { Introduces a value in a certain position }
    array_counts rot grid @ * rot + + c! ;    { of array_counts                          }

: add_counts                                  { Adds 1 to counts for every alive cell    } 
    1 =                                       { around                                   }
      if counts dup c@ 1 + swap c!
    then ;

: counting                                               { Counts the number of alive    }
    reset_counts                                         { cells around each cell        }
    value_x c@ 1 - value_y c@ 1 - array_xy_@ add_counts
    value_x c@ 1 - value_y c@ array_xy_@ add_counts
    value_x c@ 1 - value_y c@ 1 + array_xy_@ add_counts
    value_x c@ value_y c@ 1 - array_xy_@ add_counts
    value_x c@ value_y c@ 1 + array_xy_@ add_counts
    value_x c@ 1 + value_y c@ 1 - array_xy_@ add_counts
    value_x c@ 1 + value_y c@ array_xy_@ add_counts
    value_x c@ 1 + value_y c@ 1 + array_xy_@ add_counts
    counts c@ ;

: alive                                                  { Determines if an alive cell   }
    case                                                 { remains alive or dies         }
      1 of 0 value_x c@ value_y c@ array_counts_! endof
      2 of 1 value_x c@ value_y c@ array_counts_! endof
      3 of 1 value_x c@ value_y c@ array_counts_! endof
      4 of 0 value_x c@ value_y c@ array_counts_! endof
      5 of 0 value_x c@ value_y c@ array_counts_! endof
      6 of 0 value_x c@ value_y c@ array_counts_! endof
      7 of 0 value_x c@ value_y c@ array_counts_! endof
      8 of 0 value_x c@ value_y c@ array_counts_! endof
      0 value_x c@ value_y c@ array_counts_!
    endcase ;

: dead                                                   { Determines if a dead cell     }
    case                                                 { stays dead or becomes alive   }
      1 of 0 value_x c@ value_y c@ array_counts_! endof
      2 of 0 value_x c@ value_y c@ array_counts_! endof
      3 of 1 value_x c@ value_y c@ array_counts_! endof
      4 of 0 value_x c@ value_y c@ array_counts_! endof
      5 of 0 value_x c@ value_y c@ array_counts_! endof
      6 of 0 value_x c@ value_y c@ array_counts_! endof
      7 of 0 value_x c@ value_y c@ array_counts_! endof
      8 of 0 value_x c@ value_y c@ array_counts_! endof
      0 value_x c@ value_y c@ array_counts_!
    endcase ;



{ --------------------------------------- Matrix --------------------------------------- }

: next_line                                   { Determines the line break of the array   }            
    I 1 + grid @ mod 0 = if cr then ;         { creating a matrix                        }

: matrix_xy                                                { Matrix from array_xy        }
    cr grid_do 0 do array_xy I + c@ . next_line loop ;
: matrix_counts                                            { Matrix from array_counts    }
    cr grid_do 0 do array_counts I + c@ . next_line loop ;



{ --------------------------------------- Testing -------------------------------------- }

: test_functioning                               { Tests the functioning of the system.  }
    grid @ 0 do grid @ 0 do values_x_y I J       { Changes the values of matrix_xy        }
    value_x @ value_y @ array_xy_@ 1 = 
      if counting alive 
      else counting dead 
    then loop loop 
    grid @ 0 do grid @ 0 do values_x_y I J       { Changes the values of matrix_counts   }        
    value_x c@ value_y c@ array_counts_@
    value_x c@ value_y c@ array_xy_! loop loop ; 



{ ---------------------------------- Description bmp ----------------------------------- }

grid @ bmp-x-size !                            { Set x size of bmp in pixels             }
grid @ bmp-y-size !                            { Set y size of bmp in pixels             }
bmp-x-size @ 4 / 1 max 4 * bmp-x-size !        { Trim x-size to integer product of 4     }
bmp-x-size @ bmp-y-size @ * bmp-size !         { Find number of pixels in bmp            }
bmp-size   @ 3 * 54 +       bmp-length !       { Find length of bmp in chars inc. header }
100 bmp-x-start !                              { Set x position of upper left corner     }
100 bmp-y-start !                              { Set y position of upper left corner     }



{ -------------------------- Words to create a bmp file in memory ---------------------- }

: Make-Memory-bmp  ( x y  -- addr )        { Create 24 bit (RGB) bitmap in memory        }
  0 Locals| bmp-addr y-size x-size |
  x-size y-size * 3 * 54 +                 { Find number of bytes required for bmp file  }
  chars allocate                           { Allocate memory = 3 x size + header in chars}
  drop to bmp-addr
  bmp-addr                                 { Set initial bmp pixels and header to zero   }
  x-size y-size * 3 * 54 + 0 fill

  { Create the bmp file header block }

  66 bmp-addr  0 + c!                      { Create header entries - B                   }
  77 bmp-addr  1 + c!                      { Create header entries - M                   }
  54 bmp-addr 10 + c!                      { Header length of 54 characters              } 
  40 bmp-addr 14 + c!   
   1 bmp-addr 26 + c!
  24 bmp-addr 28 + c!                      { Set bmp bit depth to 24                     }
  48 bmp-addr 34 + c!
 117 bmp-addr 35 + c!
  19 bmp-addr 38 + c!
  11 bmp-addr 39 + c!
  19 bmp-addr 42 + c!
  11 bmp-addr 43 + c!
 
  x-size y-size * 3 * 54 +                 { Store file length in header as 32 bit Dword }
  bmp-addr 2 + !
  x-size                                   { Store bmp x dimension in header             }
  bmp-addr 18 + ! 
  y-size                                   { Store bmp y dimension in header             }
  bmp-addr 22 + ! 
  bmp-addr                                 { Leave bmp start address on stack at exit    }
  ;



{ ------------------------------ Stand Alone Test Routines ----------------------------- }


: Setup-Test-Memory                              { Create bmps in memory to start with   }
  bmp-x-size @ bmp-y-size @ make-memory-bmp
  bmp-address ! 
  cr ." Created Test bmp " cr
  ;

  Setup-Test-Memory  



{ ------------------- Word to display a bmp using MS Windows API Calls ----------------- }


Function: SetDIBitsToDevice ( a b c d e f g h i j k l -- res )

: MEM-bmp ( addr -- )                    { Prints bmp starting at address to screen      }
   [OBJECTS BITMAP MAKES BM OBJECTS]
   BM bmp!
   HWND GetDC ( hDC )
   DUP >R ( hDC ) 1 1 ( x y )            { (x,y) upper right corner of bitmap            }
   BM Width @ BM Height @ 0 0 0
   BM Height @ BM Data
   BM InfoHeader DIB_RGB_COLORS SetDIBitsToDevice DROP  { Windows API calls              }   
   HWND R> ( hDC ) ReleaseDC DROP ;



{ --------------------- bmp Display Window Class and Application ----------------------- }



0 VALUE bmp-hApp                { Variable to hold handle for default bmp display window }

: bmp-Classname Z" Show-bmp" ;  { Classname for the bmp output class                     }


: bmp-End-App ( -- res )
   'MAIN @ [ HERE CODE> ] LITERAL < IF ( not an application yet )
      0 TO bmp-hApp
   ELSE ( is an application )
      0 PostQuitMessage DROP
   THEN 0 ;


[SWITCH bmp-App-Messages DEFWINPROC ( msg -- res ) WM_DESTROY RUNS bmp-End-App SWITCH]


:NONAME ( -- res ) MSG LOWORD bmp-App-Messages ; 4 CB: bmp-APP-WNDPROC { Link window messages to process }


: bmp-APP-CLASS ( -- )
      0  CS_OWNDC   OR                  \ Allocates unique device context for each window in class
         CS_HREDRAW OR                  \ Window to be redrawn if movement / size changes width
         CS_VREDRAW OR                  \ Window to be redrawn if movement / size changes height
      bmp-APP-WNDPROC                   \ wndproc
      0                                 \ class extra
      0                                 \ window extra
      HINST                             \ hinstance
      HINST 101  LoadIcon 
   \   NULL IDC_ARROW LoadCursor        \ Default Arrow Cursor
      NULL IDC_CROSS LoadCursor         \ Cross cursor
      WHITE_BRUSH GetStockObject        \
      0                                 \ no menu
      bmp-Classname                     \ class name
   DefineClass DROP
  ;


: bmp-window-shutdown     { Close bmp display window and unregister classes on shutdown  }               
   bmp-hApp IF 
   bmp-hApp WM_CLOSE 0 0 SendMessage DROP
   THEN
   bmp-Classname HINST UnregisterClass DROP
  ;


bmp-APP-CLASS                   { Call class for displaying bmp's in a child window      }

13 IMPORT: StretchDIBits

11 IMPORT: SetDIBitsToDevice 



{ ---------------------------- bmp Window Output Routines ------------------------------ }


: New-bmp-Window-Copy  ( -- res )            \ Window class for "copy" display 
   0                                         \ exended style
   bmp-Classname                             \ class name
   s" BMP Window " pad zplace                \ window title - including bmp number
   1  (.) pad zappend pad
   WS_OVERLAPPEDWINDOW                       \ window style
   bmp-x-start @ bmp-y-start @               \ x   y Window position
   bmp-x-size @ 19 + bmp-y-size @ 51 +       \ cx cy Window size
   0                                         \ parent window
   0                                         \ menu
   HINST                                     \ instance handle
   0                                         \ creation parameters
   CreateWindowEx 
   DUP 0= ABORT" create window failed" 
   DUP 1 ShowWindow DROP
   DUP UpdateWindow DROP 
   ;


: bmp-to-screen-copy  ( n -- )            { Writes bmp at address to window with hwnd    }
  bmp-window-handle @ GetDC               { handle of device context we want to draw in  }
  2 2                                     { x , y of upper-left corner of dest. rect.    }
  bmp-x-size @ 3 -  bmp-y-size @          { width , height of source rectangle           }
  0 0                                     { x , y coord of source rectangle lower left   }
  0                                       { First scan line in the array                 }
  bmp-y-size @                            { number of scan lines                         }
  bmp-address @ dup 54 + swap 14 +        { address of bitmap bits, bitmap header        }
  0
  SetDIBitsToDevice drop
  ;


: New-bmp-Window-Stretch  ( -- res )         \ Window class for "stretch" display 
   0                                         \ exended style
   bmp-Classname                             \ class name
   s" BMP Window " pad zplace                \ window title - including bmp number
   1  (.) pad zappend pad
   WS_OVERLAPPEDWINDOW                       \ window style
   bmp-x-start @ bmp-y-start @               \ x   y Window position
   bmp-x-size @ 250 max 10 + 
   bmp-y-size @ 250 max 49 +                 \ cx cy Window size, min start size 250x250
   0                                         \ parent window
   0                                         \ menu
   HINST                                     \ instance handle
   0                                         \ creation parameters
   CreateWindowEx 
   DUP 0= ABORT" create window failed" 
   DUP 1 ShowWindow DROP
   DUP UpdateWindow DROP 
   ;

: bmp-to-screen-stretch  ( n addr -- )    { Stretch bmp at addr to window n              }
  0 0 0 
  Locals| bmp-win-hWnd bmp-win-x bmp-win-y bmp-address |
  bmp-window-handle @
  dup to bmp-win-hWnd                     { Handle of device context we want to draw in  }
  PAD GetClientRect DROP                  { Get x , y size of window we draw to          }
  PAD @RECT 
  to bmp-win-y to bmp-win-x
  drop drop                             
  bmp-win-hWnd GetDC                      { Get device context of window we draw to      }
  2 2                                     { x , y of upper-left corner of dest. rect.    }   
  bmp-win-x 4 - bmp-win-y 4 -             { width, height of destination rectangle       }
  0 0                                     { x , y of upper-left corner of source rect.   }
  bmp-address 18 + @                      { Width of source rectangle                    }
  bmp-address 22 + @                      { Height of source rectangle                   }
  bmp-address dup 54 + swap 14 +          { address of bitmap bits, bitmap header        }
  0                                       { usage                                        }
  13369376                                { raster operation code                        } 
  StretchDIBits drop
  ; 



{ ------------------------------------ Storing bmp image ------------------------------- }


: temp-directory                     { leave start address + length of string on stack   }
  s" C:\Users\jumar\Desktop\ICL\3 year\Labs\"  { for bmp directory path                  }
  ;


: default-bmp-name                   { leave start address + length of string on stack    }
  s" bmp_output_0001.bmp"            { for bmp name                                       }
  ;


: default-bmp-path ( -- addr n )     { leave start address + length of string on stack    }
  s" C:\Users\jumar\Desktop\ICL\3 year\Labs\bmp_output_0001.bmp"    { for bmp path + file name                             }
  ;


: save_bmp_file   ( -- )             { Save a bmp file starting at bmp-address to disk    }
  bmp-address dup @ swap             { using default bmp-path Will overwrite files        }
  @ 2 + @
  default-bmp-path 
  R/W create-file drop dup pad !
  write-file 
  0= if    cr    s" File successfuly saved " type cr
     else  cr    s" File not saved " type cr
     then
  pad @ close-file drop
  ;



{ -------------------------------------- Storing data ----------------------------------- } 

: make-test-file                                  { Create a test file to read / write to }
  s" C:\Users\jumar\Desktop\ICL\3 year\Labs\data_0002.dat" r/w create-file drop  { Create the file                        } 
  test-file-id !                                  { Store file handle for later use       }
;

 
: open-test-file                                  { Open the file for read/write access   }
  s" C:\Users\jumar\Desktop\ICL\3 year\Labs\data_0002.dat" r/w open-file drop    { Not needed if we have just created     }
  test-file-id !                                  { file.                                 }
;


: close-test-file                                 { Close the file pointed to by the file }
  test-file-id @                                  { handle.                               }
  close-file drop
; 


: test-file-size                                  { Leave size of file on top of stack as }
  test-file-id @                                  { a double prescision integer if the    }
  file-size drop                                  { file is open.                         }
;


: write-file-header 
  s"   Matrix" test-file-id @ write-line drop     { Writes single lines of text to a file }
                                                  { terminating each line with a LF/CR.   }
                                                  { The file must be open for R/W access  }
                                                  { first.                                }
 ;


: write-file-data-1                               { Writes data in the file               }
  write-line drop                                 
;

: write-file-data-2                               { Writes data in the file               }
  write-line drop                                 
;
    
: Write-blank-data                                { Write an empty line to the file       }
  s"  " test-file-id @ write-line drop
;


: save_data                                       { Saves the data in the file            }
  make-test-file
  write-file-header
  write-file-data-1
  Write-blank-data
  write-file-data-2
  Write-blank-data
  close-test-file
  cr cr
;



{ ---------------------------------- Setting image grid --------------------------------- }


: Reset-bmp-Pixels  ( addr -- )  { Set all color elements of bmp at addr to zero = black }
  dup 54 + swap
  2 + @ 54 - 0 fill
  ;

: map_bmp                               { Creates the colour combination to form white    }
  grid @ dup * 3 * 0 do I               { and black grid .bmp image                       }
  3 / array_xy + c@ 255 * 
  bmp-address @ 54 + I + c! loop ;

: set_bmp                               { Creates the .bmp image                          }
  New-bmp-Window-stretch                
  bmp-window-handle !
  bmp-address @ Reset-bmp-pixels
  bmp-address @ bmp-to-screen-stretch
  ;



{ ------------------------------ Creation of the .bmp image ----------------------------- }

: selected                            { Selects the places in array_xy where the values   }
  1 array_xy 0 + c!                   { want to be changed manually                       }
  1 array_xy 1 + c!
  1 array_xy 2 + c! ;

: go_create_random                    { Creates the .bmp image with random values of      }
  set_bmp                             { the matrix_xy                                     }
  reset_arrays
  random_generation
  begin
  map_bmp                             
  test_functioning
  bmp-address @ bmp-to-screen-stretch { Stretch .bmp to display window                    }
  1000 ms                             { Delay for viewing ease, reduce for higher speed   }
  key?                                { Break test loop on key press                      }                                
  until                               
  ; 
  
: go_create_selected                  { Creates the .bmp image with manually chosen       }
  set_bmp                             { values for matrix_xy                              }
  reset_arrays
  selected 
  begin
  map_bmp                             
  test_functioning
  bmp-address @ bmp-to-screen-stretch 
  1000 ms  
  key?                               
  until
  ;  

go_create_random