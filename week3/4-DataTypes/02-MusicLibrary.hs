type Date = String
type Title  = String
type Genre   = String
type Length = Int

data MusicLibrary = MusicLibrary Song MusicLibrary | Empty
    deriving (Show)

data RecordLabel = Unknown | RecordLabel1
    deriving (Enum, Show, Eq)

data Author = Author {
    authorName :: String,
    authorBirthDate :: Date,
    authorRecordLabel :: RecordLabel
} deriving (Show, Eq)

data Song = Song {
    songTitle :: Title,
    songAuthor :: Author,
    songGenre :: Genre,
    songLength :: Length
} deriving (Show, Eq)

author = Author { authorName = "Joe", authorBirthDate = "03.03.1956", authorRecordLabel = Unknown }
song1 = Song { songTitle = "ElectroRaid", songAuthor = author, songGenre = "TestGenre", songLength = 256 }
song2 = Song { songTitle = "Thunderdome", songAuthor = author, songGenre = "OtherGenre", songLength = 345 }

library = Empty

-- adding a song to the library

addSongToLibrary :: Song -> MusicLibrary -> MusicLibrary
addSongToLibrary s lib = MusicLibrary s lib

lib1 = addSongToLibrary song1 library
lib2 = addSongToLibrary song2 lib1

-- removing a song from the library

removeSongFromLibrary :: Song -> MusicLibrary -> MusicLibrary
removeSongFromLibrary s (MusicLibrary s1 lib) | s == s1 = lib
                                              | otherwise = MusicLibrary s1 (removeSongFromLibrary s lib)
removeSongFromLibrary s _ = Empty

lib3 = removeSongFromLibrary song1 lib2

-- removing all songs by an author

removeAllSongsByAuthorFromLibrary :: Author -> MusicLibrary -> MusicLibrary
removeAllSongsByAuthorFromLibrary author (MusicLibrary s1 lib) | songAuthor s1 == author = lib
                                                               | otherwise = MusicLibrary s1 (removeAllSongsByAuthorFromLibrary author lib)
author2 = Author { authorName = "Ralf", authorBirthDate = "03.10.1966", authorRecordLabel = RecordLabel1 }
songWithNewAuthor = Song { songTitle = "OtherMusic", songAuthor = author2, songGenre = "TestGenre", songLength = 256 }

libWithNewAuthor = addSongToLibrary songWithNewAuthor lib3
libWithoutNewAuthor = removeAllSongsByAuthorFromLibrary author2 libWithNewAuthor

-- searching for song information by title, album or author

searchForSongInfoByAuthor :: Author -> MusicLibrary -> [Song]
searchForSongInfoByAuthor author (MusicLibrary s1 lib) | songAuthor s1 == author = s1 : searchForSongInfoByAuthor author lib
                                                       | otherwise = searchForSongInfoByAuthor author lib
searchForSongInfoByAuthor _ _ = []

searchForSongInfoByTitle :: Title -> MusicLibrary -> [Song]
searchForSongInfoByTitle title (MusicLibrary s1 lib) | songTitle s1 == title = s1 : searchForSongInfoByTitle title lib
                                                     | otherwise = searchForSongInfoByTitle title lib
searchForSongInfoByTitle _ _ = []

searchForSongInfoByGenre :: Genre -> MusicLibrary -> [Song]
searchForSongInfoByGenre genre (MusicLibrary s1 lib) | songGenre s1 == genre = s1 : searchForSongInfoByGenre genre lib
                                                     | otherwise = searchForSongInfoByGenre genre lib
searchForSongInfoByGenre _ _ = []

libWithDuplicateGenres = addSongToLibrary songWithNewAuthor lib2

-- getting information about an author

getInformationAboutAuthor :: String -> MusicLibrary -> Author
getInformationAboutAuthor name (MusicLibrary s1 lib) | authorName foundAuthor == name = foundAuthor
                                                     | otherwise = getInformationAboutAuthor name lib
  where foundAuthor = songAuthor s1
getInformationAboutAuthor _ _ = error "no author information matches query"