#' Title Quantification of within-sample genetic heterogeneity from SNP-array data
#'
#' @param data matrix of segments, described below
#' @param minprobes Minimum number of probes for a segment to be included in the calculation (default 100).
#' @param minkb Minimum segment size (in kb) for a segment to be included in the calculation (default 1000, i.e. 1Mb).
#' @param nbins Number of bins for the calculation of logR-based shannon diversity (S score, default 10).
#' @param method Which measure is to be calculated, either "shannon" (default) for the Shannon diversity based S score, "simpson" for a Simpson diversity alternative or "ripley" for the Ripley's L-based L score.
#' @param norm Should the data be normalised from 0 to 1 (default TRUE).
#' @param delog Should the logRs be converted to floating point ratio estimates (2^logR, default FALSE).
#' @param ncores Number of cores to be used (default 1). If more than 1, mclapply from the parallel package will be used.
#' @param fixedmin Minimum value used for capping logRs. Not set by default. If set, will be considered as minimum possible value for bin creation.
#' @param fixedmax Maximum value used for capping logRs. Not set by default. If set, will be considered as maximum possible value for bin creation.
#' @param maxrad Maximum radius for the Ripley L calculation (R score, default 0.05).
#' @param radinc Radius increment for the Ripley L calculation (R score, default 0.001).
#'
# Format of input 'data' object
## 7-Columns matrix: SampleID, Chr, Start, End, nProbes, mBAF, logR
## Each row corresponds to a genomic segment
## SampleID: A unique identifier for each sample
## Chr: The chromosome on which the segment is located
## Start: Location of the first base pair of the segment on the chromosome
## End: Location of the last base pair of the segment on the chromosome
## nProbes: Number of probes covered by the segment
## mBAF: Mean mirrored B Allele Frequency value of all probes in the segment
## logR: Mean logR value of all probes in the segment

#' @return
#' @export
#'
#' @examples
snpDiv <- function(data, minprobes=100, minkb=1000, nbins=10,
                   method="shannon", norm=TRUE, delog=FALSE, ncores=1,
                   fixedmin=NA, fixedmax=NA, maxrad=0.05, radinc=0.001) {

  library(vegan)
  library(spatstat)
  library(parallel)

  sng_div_cols <- c("SampleID", "Chr", "Start", "End", "nProbes", "mBAF", "logR")
  methods <- c("ripley", "shannon", "simpson")

  ## Argument check
  if (length(grep(FALSE, sng_div_cols %in% colnames(data))) != 0) {
    print(paste("Error, data must contain the following columns:", paste(sng_div_cols, collapse=" ")))
    return(NULL)
  }
  if (as.numeric(minprobes) < 1 | is.na(as.numeric(minprobes))) {
    print("Error: minprobes argument must be numeric and > 0")
    return(NULL)
  }
  if (as.numeric(minkb) < 0.001 | is.na(as.numeric(minkb))) {
    print("Error: minkb argument must be numeric and >= 0.001")
    return(NULL)
  }
  if (method %in% c("shannon", "simpson")) {
    if (as.numeric(nbins) <= 2 | is.na(as.numeric(nbins))) {
      print("Error: nbins argument must be numeric and >= 2")
      return(NULL)
    }
  }
  if (!method %in% methods) {
    print(paste("Error, possible values for method argument are:", paste(methods, collapse=" ")))
    return(NULL)
  }

  ## Create data frame
  segments <- data %>%
    dplyr::mutate(SampleID = as.character(SampleID)) %>%
    dplyr::mutate(Chr = as.numeric(Chr)) %>%
    dplyr::mutate(Start = as.numeric(Start)) %>%
    dplyr::mutate(End = as.numeric(End)) %>%
    dplyr::mutate(nProbes = as.numeric(nProbes)) %>%
    dplyr::mutate(mBAF = as.numeric(mBAF)) %>%
    dplyr::mutate(logR = as.numeric(logR)) %>%
    dplyr::filter(nProbes >= minprobes & ((End - Start) >= (minkb * 1000)))

  # segments=data.frame(SampleID=as.character(data[,"SampleID"]),
  #                     Chr=as.numeric(data[,"Chr"]), Start=as.numeric(data[,"Start"]), End=as.numeric(data[,"End"]),
  #                     nProbes=as.numeric(data[,"nProbes"]), mBAF=as.numeric(data[,"mBAF"]),
  #                     logR=as.numeric(data[,"logR"]))
  # segments <- segments[which(segments$nProbes >= minprobes &
  #                              ((segments$End - segments$Start) >= (minkb * 1000))),]

  # Check if segments is empty, return NULL. by wenchuan.xie@20240321
  if(nrow(segments) == 0) {
    print(paste("Error, no segment data after filtered"))
    return(NULL)
  }
  ## Sanity checks
  ## mean BAFs should be mirrored (>0.5). Do it if it's not the case.
  if (method == "ripley") {
    tmpidx <- which(segments$mBAF < 0.5)
    if (length(tmpidx) > 0) {
      print("Warning: some segments have mBAF < 0.5 - shouldn't be the case. These will be mirrored.")
      segments$mBAF[tmpidx] <- 0.5 + abs(segments$mBAF[tmpidx] - 0.5)
    }
  }
  ## Capping logRs if needed
  if (!is.na(fixedmin)) {
    segments$logR <- unlist(lapply(segments$logR, function(x){max(x,fixedmin)}))
  }
  if (!is.na(fixedmax)) {
    segments$logR <- unlist(lapply(segments$logR, function(x){min(x,fixedmax)}))
  }

  ## Setting maxrad to 0.05 if undefined
  if (is.na(as.numeric(maxrad))) {
    maxrad=0.05
  }
  if (is.na(as.numeric(radinc))) {
    maxrad=0.001
  }

  ## Delog logRs if asked
  if (delog == T) {
    segments$logR <- 2 ^ segments$logR
  }

  sids <- unique(segments$SampleID)

  if (ncores == 1) {
    res <- unlist(lapply(sids, calcSnpDiv, segments=segments, nbins=nbins, method=method, norm=norm,
                         maxrad=maxrad, radinc=radinc, fixedmin=fixedmin, fixedmax=fixedmax))
  } else {
    res <- unlist(mclapply(sids, calcSnpDiv, segments=segments, nbins=nbins, method=method, norm=norm,
                           maxrad=maxrad, radinc=radinc, fixedmin=fixedmin, fixedmax=fixedmax, mc.cores=ncores))
  }
  names(res) <- sids
  return(res)
}
## Calculate the R score
calcRipleysL <- function(segmentList, norm, maxrad, radinc) {
  if (norm == T) {
    ## convert segmentList to a "planer point pattern" for spapstat
    ## normalise BAFs and logRs
    segmentList[,"mBAF"] <- segmentList[,"mBAF"] - min(segmentList[,"mBAF"], na.rm=T)
    segmentList[,"mBAF"] <- segmentList[,"mBAF"] / max(segmentList[,"mBAF"], na.rm=T)
    segmentList[,"logR"] <- segmentList[,"logR"] - min(segmentList[,"logR"], na.rm=T)
    segmentList[,"logR"] <- segmentList[,"logR"] / max(segmentList[,"logR"], na.rm=T)
    ## convert segmentList to a "planer point pattern" for spapstat
    pp <- as.ppp(segmentList, c(0,1,0,1))
    ## calc Ripley's K
    ripleyL <- Lest(pp,correction="isotropic", r=seq(0, maxrad, radinc))
  } else {
    ## convert segmentList to a "planer point pattern" for spapstat. Limit to 0:1 for BAF, -3:3 for logR.
    pp <- as.ppp(segmentList, c(0,1,-3,3))
    ## calc Ripley's K
    ripleyL <- Lest(pp,correction="isotropic");
    ## calc difference from theoretical (Poisson) random process
  }
  return(ripleyL)
}
## Calculate number of values in given bin
ninbin <- function(bin, data, binmin, binmax, binsize) {
  length(which(data >= seq(binmin, binmax, binsize)[bin] &
                 data < seq(binmin, binmax, binsize)[bin+1]))
}
## Subfunction, calculate the score per sample.
calcSnpDiv <- function(sampleid, segments, nbins, method, norm, maxrad, radinc, fixedmin, fixedmax) {
  ##print(sampleid)
  sidsegs <- segments[which(segments$SampleID == sampleid),]

  ## Ripley's L
  if (method == "ripley") {
    sidsegs <- sidsegs[which(!is.na(sidsegs$mBAF) & !is.na(sidsegs$logR)),,drop=F]
    if (nrow(sidsegs) == 0) {
      return(NA)
    }
    tmp <- calcRipleysL(sidsegs[,c("mBAF", "logR")], norm=norm, maxrad, radinc)
    return(sum(tmp$iso-tmp$theo))
  }

  ## Shannon/simpson diversity indexes
  if (method %in% c("shannon", "simpson")) {
    sidsegs <- sidsegs[which(!is.na(sidsegs$mBAF) & !is.na(sidsegs$logR)),,drop=F]
    if (nrow(sidsegs) == 0) {
      return(NA)
    }
    stat = sidsegs$logR
    binmax=ifelse(is.na(fixedmax), max(stat, na.rm=T), fixedmax)
    binmin=ifelse(is.na(fixedmin), min(stat, na.rm=T), fixedmin)
    binsize <- (binmax-binmin)/nbins
    bins <- unlist(lapply(1:nbins, ninbin, data=stat, binmin=binmin, binmax=binmax, binsize=binsize))
    bins[nbins] = bins[nbins] + length(which(stat==binmax))
    ## Return diversity method (vegan library)
    return(vegan::diversity(bins, index=method))
  }
  return(NA)
}


#' Title
#'
#' @param seg_data
#'
#' @return
#' @export
#'
#' @examples
ldy_snpDiv <- function(seg_data, minprobes=100, minkb=1000, nbins=10){

  if(length(grep(FALSE, c('X','Y') %in% colnames(seg_data))) != 0){
    seg_data <- seg_data %>%
      dplyr::filter(!Chr %in% c('X','Y'))
  }

  # in_seg_data <- na.omit(in_seg_data)
  # Shannon Index (S score)
  shnnonindex <- snpDiv(seg_data,method = "shannon",minprobes = minprobes, minkb = minkb, nbins = nbins,norm = T)
  # Evenness Index
  evennessindex <- shnnonindex/max(shnnonindex)
  # Simpson Index（lambda）
  simpsonindex <- snpDiv(seg_data,method = "simpson",minprobes = minprobes, minkb = minkb, nbins = nbins)
  # R score
  rscore <- snpDiv(seg_data,method = "ripley",minprobes = minprobes, minkb = minkb, nbins = nbins)

  out <- data.frame('SampleID' = names(shnnonindex),
                    "Shannon_index" = shnnonindex,
                    "Evenness_index" = evennessindex,
                    "Simpson_index" = simpsonindex,
                    "Ripley_index" = rscore) %>%
    tibble::remove_rownames() %>%
    dplyr::mutate_at(.vars = setdiff(colnames(.),"SampleID"),.funs = function(x) round(x,4))

  return(out)
}
