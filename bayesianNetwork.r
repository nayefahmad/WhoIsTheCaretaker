#source("http://bioconductor.org/biocLite.R")
#biocLite("Rgraphviz") # Rgraphviz requires a separata installation

library(bnlearn)
set.seed(3)
net <- model2network("[HusbandDemographics][HusbandIsProfessional][NannyDemographics][WifeDemographics|HusbandDemographics][StayAtHomeMom|HusbandIsProfessional:WifeDemographics][HouseholdHasNanny|StayAtHomeMom:HusbandIsProfessional][Caretaker|StayAtHomeMom:HouseholdHasNanny][CaretakerEthnicity|WifeDemographics:Caretaker:NannyDemographics]")

plot(net)

yn <- c("yes", "no")
ca <- c("caucacian","other")
ao <- c("asian","other")
nw <- c("nanny","wife")

cptHusbandDemographics <- matrix(c(0.85, 0.15), ncol=2, dimnames=list(NULL, ca)) #[1]
cptHusbandIsProfessional <- matrix(c(0.81, 0.19), ncol=2, dimnames=list(NULL, yn)) #[2]
cptNannyDemographics <- matrix(c(0.06, 0.94), ncol=2, dimnames=list(NULL, ao)) # [3]
cptWifeDemographics <- matrix(c(0.01, 0.99, 0.33, 0.67), ncol=2, dimnames=list("WifeDemographics"=ao, "HusbandDemographics"=ca)) #[1]
cptStayAtHomeMom <- c(0.3, 0.7, 0.14, 0.86, 0.125, 0.875, 0.125, 0.875) #[4]

dim(cptStayAtHomeMom) <- c(2, 2, 2)
dimnames(cptStayAtHomeMom) <- list("StayAtHomeMom"=yn, "WifeDemographics"=ao, "HusbandIsProfessional"=yn)

cptHouseholdHasNanny <- c(0.01, 0.99, 0.035, 0.965, 0.00134, 0.99866, 0.00134, 0.99866) #[5]
dim(cptHouseholdHasNanny) <- c(2, 2, 2)
dimnames(cptHouseholdHasNanny) <- list("HouseholdHasNanny"=yn, "StayAtHomeMom"=yn, "HusbandIsProfessional"=yn)

cptCaretaker <- c(0.5, 0.5, 0.999, 0.001, 0.01, 0.99, 0.001, 0.999)
dim(cptCaretaker) <- c(2, 2, 2)
dimnames(cptCaretaker) <- list("Caretaker"=nw, "StayAtHomeMom"=yn, "HouseholdHasNanny"=yn)

cptCaretakerEthnicity <- c(0.99, 0.01, 0.99, 0.01, 0.99, 0.01, 0.01, 0.99, 0.01,0.99,0.99,0.01,0.01,0.99,0.01,0.99)
dim(cptCaretakerEthnicity) <- c(2, 2, 2,2)
dimnames(cptCaretakerEthnicity) <- list("CaretakerEthnicity"=ao,"Caretaker"=nw, "WifeDemographics"=ao ,"NannyDemographics"=ao)

net.disc <- custom.fit(net, dist=list(HusbandDemographics=cptHusbandDemographics, HusbandIsProfessional=cptHusbandIsProfessional, WifeDemographics=cptWifeDemographics, StayAtHomeMom=cptStayAtHomeMom, HouseholdHasNanny=cptHouseholdHasNanny, Caretaker=cptCaretaker, NannyDemographics=cptNannyDemographics,CaretakerEthnicity=cptCaretakerEthnicity))

#set.seed(3)
probWife <- cpquery(net.disc, (Caretaker=="wife"),HusbandDemographics=="caucacian" & HusbandIsProfessional=="yes" & CaretakerEthnicity=="asian",n=1000000)
probNanny <- cpquery(net.disc, (Caretaker=="nanny"),HusbandDemographics=="caucacian" & HusbandIsProfessional=="yes" & CaretakerEthnicity=="asian",n=1000000) 

print(paste0("The probability that the caretaker is his wife is = ", probWife))
print(paste0("The probability that the caretaker is the nanny = ", probNanny))
graphviz.plot(net,highlight = NULL, layout = "dot", shape = "ellipse", main = NULL, sub = NULL)


#[1] Married couples in the United States in 2010 https://en.wikipedia.org/wiki/Interracial_marriage_in_the_United_States
#[2] Civilian labor force participation rate by age, gender, race, and ethnicity https://www.bls.gov/emp/ep_table_303.htm
#[3] Caregiver Statistics: Demographics https://www.caregiver.org/caregiver-statistics-demographics
#[4] Stay-at-Home Mothers by Demographic Group http://www.pewsocialtrends.org/2014/04/08/chapter-2-stay-at-home-mothers-by-demographic-group/
#[5] The Three Faces of Work-Family Conflict, page 9, Figure 3 https://cdn.americanprogress.org/wp-content/uploads/issues/2010/01/pdf/threefaces_exec_sum.pdf
