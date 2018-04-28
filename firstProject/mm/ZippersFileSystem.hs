
import Data.List (break)


type Name = String
type Data = String
data FSItem =File Name Data | Folder Name [FSItem] deriving (Show)

myDisk :: FSItem
myDisk =
    Folder "root"
        [File "goat_yelling_like_man.wmv" "baaaaaaaaa"
        , File "pope_time.avi" "God bless You"
        , Folder "pics"
            [ File "ape_throwing_up.jpg" "bleargh"
            , File "watermelon_smash.gif" "smash!!"
            , File "skull_man(scary).bmp" "Yaikes!"
            ]
        , File "dijon_poupon.doc" "best_mustard"
        , Folder "programs"
            [ File "fartwizard.exe" "10gotofart"
            , File "owl_bandit.dmg" "mov eax, h00t"
            , File "not_a_virus_exe" "realy_not_a_virus"
            , Folder "source code"
                [ File "best_hs_prog.hs" "main = print( fix error)"
                , File "random.hs" "main = print 4"
                ]
            ]
        ]    

-- A Crumb consists of the name (Folder or File) and two lists
-- 1.) the FSItems before name 
-- 2.) the FSItems after name
data FSCrumb = FSCrumb Name [FSItem] [FSItem] deriving (Show)
type FSZipper = (FSItem,[FSCrumb])


fsUp :: FSZipper -> FSZipper 
fsUp (item,FSCrumb name ls rs:bs)=(Folder name (ls ++ [item] ++ rs),bs)


-- The Name (-String) is matched againts name
-- 
-- the FSZipper argument is pattern matched agains the tuple of Item, list_of_breadcrubs)
    -- The FSItem is: Folder folderName items
    -- The list_of_breadcrumbs is: bs
--
-- the (list_of) items is split into two lists before and after the item with the name equal to target.
-- We build a new crumb: FSCrumb folderName before after
-- we return a FSZipper consisting of the item and the new list_of_breadcrumbs with a new first crumb
fsTo :: Name ->  FSZipper ->FSZipper
fsTo target (Folder folderName items,bs)= 
    let (before, item:after) = break (nameIs target) items
    in (item, FSCrumb folderName before after:bs)

nameIs :: Name -> FSItem -> Bool
nameIs name (Folder folderName _ ) = name == folderName
nameIs name (File fileName _ ) = name == fileName

x -: f=f x 

--example
newFocus = (myDisk,[]) -: fsTo "pics" -: fsTo "skull_man(scary).bmp"  
newFocus2 = newFocus -: fsUp -: fsTo "watermelon_smash.gif"


fsRename :: Name -> FSZipper -> FSZipper
fsRename newName (Folder name items, bs) = (Folder newName items,bs)
fsRename newName (File name dat , bs) = (File newName dat,bs)

fsNewFile :: FSItem -> FSZipper -> FSZipper
fsNewFile item (Folder folderName items, bs)=
    (Folder folderName (item:items),bs)

