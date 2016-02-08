# Split frase menggunakan regular expression
# Req: dictionary untuk membentuk frase, yaitu:
# - frase  	: list frase yg sudah lazim, spt 'ibu kota', 'jawa barat', 'ulang tahun'
# - awalan 	: list kata awalan spt 'tidak', 'sangat', dll untuk membentuk frase spt 'tidak baik', 'sangat bagus'
# - akhiran	: list kata akhiran spt 'banget', 'sekali', dll untuk membentuk frase spt 'indah banget'

split_frase <- function(text) {
  
  require(stringr)
  
  # dictionary. silakan diperkaya!
  awalan <- c("tidak","tdk","tida","tak","ga","ngga","nggak","gak","sangat","amat")
  frase <- c("jawa barat","batu bata","ulang tahun","ibu kota")
  akhiran <- c("banget","bgt","sekali","nian")
  
  regx <- paste( 
            "((?:(?:"
            , paste(awalan, collapse="|")
            , ") ){,2}(?:"
            , paste(frase, collapse="|")
            , "|[^ ]*)?(?: (?:"
            , paste(akhiran, collapse="|")
            , "))?)"
            , sep="" )
  
  word.list <-
    str_split(
      gsub(",$"
           , ""
           , gsub(", "
                  , ","
                  , gsub(regx
                         , "\\1,"
                         , text) ) ) , ",")
  
  return(word.list)

}


# Contoh:

text <- c("ibu pergi ke ibu kota untuk merayakan ulang tahun"
         , "kota bandung jawa barat sangat indah sekali"
         , "layanan di bank itu tidak memuaskan"
         , "lakukan sekali lagi")

sapply(text, split_frase)


# result :

# $`ibu pergi ke ibu kota untuk merayakan ulang tahun`
# [1] "ibu"         "pergi"       "ke"          "ibu kota"    "untuk"       "merayakan"   "ulang tahun"
#
# $`kota bandung jawa barat sangat indah sekali`
# [1] "kota"                "bandung"             "jawa barat"          "sangat indah sekali"
#
# $`layanan di bank itu tidak memuaskan`
# [1] "layanan"         "di"              "bank"            "itu"             "tidak memuaskan"
#
# $`lakukan sekali lagi`
# [1] "lakukan sekali" "lagi"

# Need improvement untuk frase yg dibentuk dg akhiran 'sekali'
# Untuk contoh [4] di atas, hasil seharusnya: "lakukan" "sekali" "lagi"

# Credit: Suharto Anggono
