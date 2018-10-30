#' getRefSeq
#'
#' get proteinsequence(s) from RefSeq-IDs
#' @export
getRefSeq <- function(id) {
    # see: http://www.ncbi.nlm.nih.gov/books/NBK25499/#chapter4.EFetch
    url <- paste(sep="",
        "http://eutils.ncbi.nlm.nih.gov/entrez/eutils/efetch.fcgi",
        "?db=protein",
        "&id=", paste(id, collapse=","),
        "&rettype=fasta",
        "&retmode=text"
    )
    return(read.fasta(url))
}

#' mapsequence
#'
#' as in part of indymed::mapProtein
#' @export
mapsequence <- function(peps, pseq) {
    if (nchar(pseq) < 15) stop("Sequence too short. Need at least 15 AAs")
    pseq <- unlist(strsplit(pseq,""),use.names=F)
    targ <- c()
    posy <- c()
    for (j in 1:(length(pseq)-14)) {
        cpep <- paste(pseq[j:(j+14)],collapse="")
        if (cpep %in% peps) {
            targ <- c(targ, cpep)
            posy <- c(posy, j)
        }
    }
    names(targ) <- posy
    return (targ)
}

#' peptize
#'
#' split protein-sequnece to multiple peptides
#' @export
peptize <- function(sq, len=15, shift=4) {
    stopifnot(nchar(sq)>=len)
    if (shift > len) warning("Shift > Length. There will be gaps.")
    # calculate start-positions.
    spos <- 0:floor((nchar(sq)-len)/shift)*shift
    # most likely, the last one is missing and last shift will be smaller.
    if (tail(spos,1)+len < nchar(sq)) spos <- c(spos, nchar(sq)-len)
    # generate peptides, assign startpositions as names
    peps <- sapply(spos, function(x){substr(sq, x+1, x+len)})
    names(peps) <- spos
    return(peps)
}

#' @export
AAprops <- function() { return(data.frame(
    Code=          c("A",         "C",         "D",              "E",             "F",             "G",         "H",           "I",          "K",         "L",         "M",          "N",          "P",            "Q",         "R",              "S",         "T",         "V",         "W",          "Y"),
    Name_DE=       c("Alanin",    "Cystein",   "Asparaginsäure", "Glutaminsäure", "Phenylalanin",  "Glycin",    "Histidin",    "Isoleucin",  "Lysin",     "Leucin",    "Methionin",  "Asparagin",  "Prolin",       "Glutamin",  "Arginin",        "Serin",     "Threonin",  "Valin",     "Tryptophan", "Tyrosin"),
    Name_EN=       c("Alanine",   "Cysteine",  "Aspartic acid",  "Glutamic acid", "Phenylalanine", "Glycine",   "Histidine",   "Isoleucine", "Lysine",    "Leucine",   "Methionine", "Asparagine", "Proline",      "Glutamine", "Arginine",       "Serine",    "Threonine", "Valine",    "Tryptophan", "Tyrosine"),
    Short=         c("Ala",       "Cys",       "Asp",            "Glu",           "Phe",           "Gly",       "His",         "Ile",        "Lys",       "Leu",       "Met",        "Asn",        "Pro",          "Gln",       "Arg",            "Ser",       "Thr",       "Val",       "Trp",        "Tyr"),
    Charakter=     c("aliphatic", "aliphatic", "aliphatic",      "aliphatic",     "aromatic",      "aliphatic", "aromatic",    "aliphatic",  "aliphatic", "aliphatic", "aliphatic",  "aliphatic",  "heterocyclic", "aliphatic", "aliphatic",      "aliphatic", "aliphatic", "aliphatic", "aromatic",   "aromatic"),
    Mass=          c( 15.03,       47.1,        59.04,            73.07,           91.13,           1.01,        81.1,          57.11,        72.13,       57.11,       75.15,        58.06,        42.08,          72.09,       100.14,           31.03,       45.06,       43.09,       130.16,       107.13),
    Volume=        c( 67,          86,          91,               109,             135,             48,          118,           124,          135,         124,         124,          96,           90,             114,         148,              73,          93,          105,         163,          141),
    Polarity=      c("unpolar",   "polar",     "polar",          "polar",         "unpolar",       "unpolar",   "polar",       "unpolar",    "polar",     "unpolar",   "unpolar",    "polar",      "unpolar",      "polar",     "polar",          "polar",     "polar",     "unpolar",   "unpolar",    "polar"),
    Hydrophobicity=c( 1.8,         2.5,         -3.5,             -3.5,            2.8,             -0.4,        -3.2,          4.5,          -3.9,        3.8,         1.9,          -3.5,         -1.6,           -3.5,        -4.5,             -0.8,        -0.7,        4.2,         -0.9,         -1.3),       
    Acidicity=     c("neutral",   "neutral",   "acidic",         "acidic",        "neutral",       "neutral",   "basic (weak)", "neutral",   "basic",     "neutral",   "neutral",    "neutral",    "neutral",      "neutral",   "basic (strong)", "neutral",   "neutral",   "neutral",   "neutral",    "neutral"),
    # These are from 2010/09_DREAM5/lustrek/Supplement S3
    Propensity_All=c( 0.7513822258,1.5473196921,0.8267772972,     0.5634742596,    1.9784639254,    0.9988963068,1.3916273282,  0.9589087163, 0.8832098467,1.0730818686,1.4742539103, 0.7999232229, 0.8122871068,   0.6771324013,1.3808401721,     0.7403431674,0.8553742016,0.9054404877,2.4634126062, 2.9608344406),
    Propensity_Cla=c( 0.6539317053,1.9877633304,0.7512519453,     0.4247585541,    2.9010666975,    0.9962530884,1.6414622581,  0.950375233,  0.8336495051,1.1142304603,1.8080152976, 0.7184197583, 0.7502332814,   0.5487274772,1.6152107702,     0.636215385, 0.788166123, 0.8672705498,3.9027670085, 5.4541493331)
))}
