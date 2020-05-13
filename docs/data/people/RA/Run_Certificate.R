country_out <- lapply(1:nrow(certificate), function(i) {
  print(i)
  rmarkdown::render("Certificates.Rmd",
                    output_file=paste0("~/Documents/github/CoronaNet/data/people/RA/Cert/",paste(i,"_"),
                                       substr(str_extract(certificate$Name[i],"[A-Za-z]+"),1,20),".pdf"),
                    params=list(Name=certificate$Name[i],country=certificate$country[i],Since=certificate$Since[i],End=certificate$End[i],Validating=certificate$Validating[i],Cleaning=certificate$Cleaning[i] )) })

