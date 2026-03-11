{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE TypeApplications #-}

import Control.Exception (bracket)
import Control.Monad.ST (runST)
import Data.ByteString.Short.Internal (ShortByteString (SBS))
import Data.Char (ord)
import Data.List.Split (splitOn)
import Data.Word (Word8)
import Gauge.Main (bench, bgroup, defaultMain, whnf)
import System.Entropy (CryptHandle, closeHandle, openHandle)

import qualified Data.Bytes as Bytes
import qualified Data.Bytes.HashMap as BH
import qualified Data.HashMap.Strict as UC
import qualified Data.List as List
import qualified Data.Primitive as PM
import qualified Data.Primitive.Unlifted.Array as PM
import qualified GHC.Exts as Exts

main :: IO ()
main = do
  fastMap <- bracket openHandle closeHandle mkFastMap
  defaultMain
    [ bgroup
        "lookup"
        [ bench "unordered-containers" $
            whnf lookupManySlow slowMap
        , bench "bytehash" $
            whnf lookupManyFast fastMap
        ]
    ]

c2w :: Char -> Word8
c2w = fromIntegral . ord

slowMap :: UC.HashMap ShortByteString Bool
slowMap =
  PM.foldlUnliftedArray'
    ( \acc token ->
        UC.insert
          (case copy token of PM.ByteArray x -> SBS x)
          (mod (PM.sizeofByteArray token) 2 == 0)
          acc
    )
    UC.empty
    tokens

mkFastMap :: CryptHandle -> IO (BH.Map Bool)
mkFastMap h =
  BH.fromList
    h
    ( map
        (\b -> (Bytes.fromByteArray (copy b), mod (PM.sizeofByteArray b) 2 == 0))
        (Exts.toList tokens)
    )

lookupManyFast :: BH.Map Bool -> Int
{-# NOINLINE lookupManyFast #-}
lookupManyFast m =
  PM.foldlUnliftedArray'
    ( \ !acc token -> case BH.lookup (Bytes.fromByteArray token) m of
        Just True -> acc + 1
        Just False -> acc + 2
        Nothing -> acc
    )
    0
    tokens

lookupManySlow :: UC.HashMap ShortByteString Bool -> Int
{-# NOINLINE lookupManySlow #-}
lookupManySlow m =
  PM.foldlUnliftedArray'
    ( \ !acc (PM.ByteArray token) -> case UC.lookup (SBS token) m of
        Just True -> acc + 1
        Just False -> acc + 2
        Nothing -> acc
    )
    0
    tokens

-- This is used to stop the pointer equality check in
-- the Eq instance of ByteArray from working.
copy :: PM.ByteArray -> PM.ByteArray
copy b = runST do
  let len = PM.sizeofByteArray b
  dst <- PM.newByteArray len
  PM.copyByteArray dst 0 b 0 len
  PM.unsafeFreezeByteArray dst

tokens :: PM.UnliftedArray PM.ByteArray
tokens =
  Exts.fromList $
    map (Exts.fromList . map c2w) $
      splitOn " " $
        List.intercalate
          " "
          [ "Enter two Sentinels-[first,] Francisco, [who paces up and down at his post; then] Bernardo, [who approaches him]."
          , "Bernardo. Who's there?"
          , "Francisco. Nay, answer me. Stand and unfold yourself."
          , "Bernardo. Long live the King!"
          , "Francisco. Bernardo?"
          , "Bernardo. He."
          , "Francisco. You come most carefully upon your hour."
          , "Bernardo. 'Tis now struck twelve. Get thee to bed, Francisco."
          , "Francisco. For this relief much thanks. 'Tis bitter cold,"
          , "And I am sick at heart."
          , "Bernardo. Have you had quiet guard?"
          , "If you do meet Horatio and Marcellus,"
          , "The rivals of my watch, bid them make haste."
          , "Enter Horatio and Marcellus."
          , "Francisco. I think I hear them. Stand, ho! Who is there?"
          , "Horatio. Friends to this ground."
          , "Marcellus. And liegemen to the Dane."
          , "Francisco. Give you good night."
          , "Marcellus. O, farewell, honest soldier."
          , "Who hath reliev'd you?"
          , "Francisco. Bernardo hath my place."
          , "Give you good night. Exit."
          , "Marcellus. Holla, Bernardo!"
          , "Bernardo. Say-"
          , "What, is Horatio there?"
          , "Horatio. A piece of him."
          , "Bernardo. Welcome, Horatio. Welcome, good Marcellus."
          , "Marcellus. What, has this thing appear'd again to-night?"
          , "Bernardo. I have seen nothing."
          , "Marcellus. Horatio says 'tis but our fantasy,"
          , "And will not let belief take hold of him"
          , "Touching this dreaded sight, twice seen of us."
          , "Therefore I have entreated him along,"
          , "With us to watch the minutes of this night,"
          , "That, if again this apparition come,"
          , "He may approve our eyes and speak to it."
          , "Horatio. Tush, tush, 'twill not appear."
          , "Bernardo. Sit down awhile,"
          , "And let us once again assail your ears,"
          , "That are so fortified against our story,"
          , "What we two nights have seen."
          , "Horatio. Well, sit we down,"
          , "And let us hear Bernardo speak of this."
          , "Bernardo. Last night of all,"
          , "When yond same star that's westward from the pole"
          , "Had made his course t' illume that part of heaven"
          , "Where now it burns, Marcellus and myself,"
          , "The bell then beating one-"
          , "Enter Ghost."
          , "Marcellus. Peace! break thee off! Look where it comes again!"
          , "Bernardo. In the same figure, like the King that's dead."
          , "Marcellus. Thou art a scholar; speak to it, Horatio."
          , "Bernardo. Looks it not like the King? Mark it, Horatio."
          , "Horatio. Most like. It harrows me with fear and wonder."
          , "Bernardo. It would be spoke to."
          , "Marcellus. Question it, Horatio."
          , "Horatio. What art thou that usurp'st this time of night"
          , "Together with that fair and warlike form"
          , "In which the majesty of buried Denmark"
          , "Did sometimes march? By heaven I charge thee speak!"
          , "Marcellus. It is offended."
          , "Bernardo. See, it stalks away!"
          , "Horatio. Stay! Speak, speak! I charge thee speak!"
          , "Exit Ghost."
          , "Marcellus. 'Tis gone and will not answer."
          , "Bernardo. How now, Horatio? You tremble and look pale."
          , "Is not this something more than fantasy?"
          , "What think you on't?"
          , "Horatio. Before my God, I might not this believe"
          , "Without the sensible and true avouch"
          , "Of mine own eyes."
          , "Marcellus. Is it not like the King?"
          , "Horatio. As thou art to thyself."
          , "Such was the very armour he had on"
          , "When he th' ambitious Norway combated."
          , "So frown'd he once when, in an angry parle,"
          , "He smote the sledded Polacks on the ice."
          , "'Tis strange."
          , "Marcellus. Thus twice before, and jump at this dead hour,"
          , "With martial stalk hath he gone by our watch."
          , "Horatio. In what particular thought to work I know not;"
          , "But, in the gross and scope of my opinion,"
          , "This bodes some strange eruption to our state."
          , "Marcellus. Good now, sit down, and tell me he that knows,"
          , "Why this same strict and most observant watch"
          , "So nightly toils the subject of the land,"
          , "And why such daily cast of brazen cannon"
          , "And foreign mart for implements of war;"
          , "Why such impress of shipwrights, whose sore task"
          , "Does not divide the Sunday from the week."
          , "What might be toward, that this sweaty haste"
          , "Doth make the night joint-labourer with the day?"
          , "Who is't that can inform me?"
          , "Horatio. That can I."
          , "At least, the whisper goes so. Our last king,"
          , "Whose image even but now appear'd to us,"
          , "Was, as you know, by Fortinbras of Norway,"
          , "Thereto prick'd on by a most emulate pride,"
          , "Dar'd to the combat; in which our valiant Hamlet"
          , "(For so this side of our known world esteem'd him)"
          , "Did slay this Fortinbras; who, by a seal'd compact,"
          , "Well ratified by law and heraldry,"
          , "Did forfeit, with his life, all those his lands"
          , "Which he stood seiz'd of, to the conqueror;"
          , "Against the which a moiety competent"
          , "Was gaged by our king; which had return'd"
          , "To the inheritance of Fortinbras,"
          , "Had he been vanquisher, as, by the same cov'nant"
          , "And carriage of the article design'd,"
          , "His fell to Hamlet. Now, sir, young Fortinbras,"
          , "Of unimproved mettle hot and full,"
          , "Hath in the skirts of Norway, here and there,"
          , "Shark'd up a list of lawless resolutes,"
          , "For food and diet, to some enterprise"
          , "That hath a stomach in't; which is no other,"
          , "As it doth well appear unto our state,"
          , "But to recover of us, by strong hand"
          , "And terms compulsatory, those foresaid lands"
          , "So by his father lost; and this, I take it,"
          , "Is the main motive of our preparations,"
          , "The source of this our watch, and the chief head"
          , "Of this post-haste and romage in the land."
          , "Bernardo. I think it be no other but e'en so."
          , "Well may it sort that this portentous figure"
          , "Comes armed through our watch, so like the King"
          , "That was and is the question of these wars."
          , "Horatio. A mote it is to trouble the mind's eye."
          , "In the most high and palmy state of Rome,"
          , "A little ere the mightiest Julius fell,"
          , "The graves stood tenantless, and the sheeted dead"
          , "Did squeak and gibber in the Roman streets;"
          , "As stars with trains of fire, and dews of blood,"
          , "Disasters in the sun; and the moist star"
          , "Upon whose influence Neptune's empire stands"
          , "Was sick almost to doomsday with eclipse."
          , "And even the like precurse of fierce events,"
          , "As harbingers preceding still the fates"
          , "And prologue to the omen coming on,"
          , "Have heaven and earth together demonstrated"
          , "Unto our climature and countrymen."
          , "[Enter Ghost again.]"
          , "But soft! behold! Lo, where it comes again!"
          , "I'll cross it, though it blast me.- Stay illusion!"
          , "[Spreads his arms.]"
          , "If thou hast any sound, or use of voice,"
          , "Speak to me."
          , "If there be any good thing to be done,"
          , "That may to thee do ease, and, grace to me,"
          , "Speak to me."
          , "If thou art privy to thy country's fate,"
          , "Which happily foreknowing may avoid,"
          , "O, speak!"
          , "Or if thou hast uphoarded in thy life"
          , "Extorted treasure in the womb of earth"
          , "(For which, they say, you spirits oft walk in death),"
          , "[The cock crows.]"
          , "Speak of it! Stay, and speak!- Stop it, Marcellus!"
          , "Marcellus. Shall I strike at it with my partisan?"
          , "Horatio. Do, if it will not stand."
          , "Bernardo. 'Tis here!"
          , "Horatio. 'Tis here!"
          , "Marcellus. 'Tis gone!"
          , "[Exit Ghost.]"
          , "We do it wrong, being so majestical,"
          , "To offer it the show of violence;"
          , "For it is as the air, invulnerable,"
          , "And our vain blows malicious mockery."
          , "Bernardo. It was about to speak, when the cock crew."
          , "Horatio. And then it started, like a guilty thing"
          , "Upon a fearful summons. I have heard"
          , "The cock, that is the trumpet to the morn,"
          , "Doth with his lofty and shrill-sounding throat"
          , "Awake the god of day; and at his warning,"
          , "Whether in sea or fire, in earth or air,"
          , "Th' extravagant and erring spirit hies"
          , "To his confine; and of the truth herein"
          , "This present object made probation."
          , "Marcellus. It faded on the crowing of the cock."
          , "Some say that ever, 'gainst that season comes"
          , "Wherein our Saviour's birth is celebrated,"
          , "The bird of dawning singeth all night long;"
          , "And then, they say, no spirit dare stir abroad,"
          , "The nights are wholesome, then no planets strike,"
          , "No fairy takes, nor witch hath power to charm,"
          , "So hallow'd and so gracious is the time."
          , "Horatio. So have I heard and do in part believe it."
          , "But look, the morn, in russet mantle clad,"
          , "Walks o'er the dew of yon high eastward hill."
          , "Break we our watch up; and by my advice"
          , "Let us impart what we have seen to-night"
          , "Unto young Hamlet; for, upon my life,"
          , "This spirit, dumb to us, will speak to him."
          , "Do you consent we shall acquaint him with it,"
          , "As needful in our loves, fitting our duty?"
          , "Let's do't, I pray; and I this morning know"
          , "Where we shall find him most conveniently."
          , "Exeunt."
          ]
