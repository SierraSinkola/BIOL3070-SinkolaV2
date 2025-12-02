Warm-up mini-Report: Mosquito Blood Hosts in Salt Lake City, Utah
================
Sierra Sinkola
2025-11-04

- [ABSTRACT](#abstract)
- [BACKGROUND](#background)
  - [Viremia](#viremia)
- [STUDY QUESTION and HYPOTHESIS](#study-question-and-hypothesis)
  - [Questions](#questions)
  - [Hypothesis](#hypothesis)
  - [Prediction](#prediction)
- [METHODS](#methods)
  - [First Analysis - Host Species of Positive and Negative Tests for
    WNV](#first-analysis---host-species-of-positive-and-negative-tests-for-wnv)
  - [Second Analysis - Generalized Linear
    Modeling](#second-analysis---generalized-linear-modeling)
- [DISCUSSION](#discussion)
  - [Interpretation of First
    Analysis](#interpretation-of-first-analysis)
  - [Interpretation of Second
    Analysis](#interpretation-of-second-analysis)
- [CONCLUSION](#conclusion)
- [REFERENCES](#references)

# ABSTRACT

This study sought out to gather more information on the spreading of
West Nile Virus in Salt Lake City. This was done by collecting
mosquitoes in the greater Salt Lake City area and using their DNA to
determine their most recent blood meal host. This was completed using
PCR and BLAST technology. It was found that house finches served as the
primary blood meal for mosquitoes in the area of interest. Statistical
analysis showed that there was a positive and significant correlation
between the number of house finch blood meals and positive tests for
West Nile Virus. From this information, it was concluded that house
finches are the primary amplifying bird species of West Nile Virus in
Salt Lake City.

# BACKGROUND

West Nile Virus, a single-stranded RNA virus, is a virus spread
primarily by mosquitoes. This virus made its way into Utah in August of
2003, but first found in the United States in 1999 (Utah Epidemiology).
Mosquitoes carry West Nile Virus and transmit it by biting a host. Some
mosquitoes have a greater preference for a blood meal from a specific
host while others will get a blood meal from almost any host they can
bite. A common blood meal for mosquitoes comes from birds, so logically,
West Nile Virus is spread heavily through birds. With the data gathered
by placing gravid traps and CO2 traps around the greater Salt Lake area,
we are better able to understand which bird species hold the highest
threat of amplifying West Nile Virus in the Salt Lake area. We are able
to extract the DNA from the blood meal in a collected mosquito and
perform PCR in order to sequence the DNA and determine the host species.
West Nile Virus poses a threat to humans as it has the ability to lead
to sometimes deadly brain and spinal cord inflammation. If we are able
to identify where serious threats of West NileVirus are coming from, we
can better protect human safety. The plot below displays the viremia
duration in various bird species.

## Viremia

``` r
# Manually transcribe duration (mean, lo, hi) from the last table column
duration <- data.frame(
  Bird = c("Canada Goose","Mallard", 
           "American Kestrel","Northern Bobwhite",
           "Japanese Quail","Ring-necked Pheasant",
           "American Coot","Killdeer",
           "Ring-billed Gull","Mourning Dove",
           "Rock Dove","Monk Parakeet",
           "Budgerigar","Great Horned Owl",
           "Northern Flicker","Blue Jay",
           "Black-billed Magpie","American Crow",
           "Fish Crow","American Robin",
           "European Starling","Red-winged Blackbird",
           "Common Grackle","House Finch","House Sparrow"),
  mean = c(4.0,4.0,4.5,4.0,1.3,3.7,4.0,4.5,5.5,3.7,3.2,2.7,1.7,6.0,4.0,
           4.0,5.0,3.8,5.0,4.5,3.2,3.0,3.3,6.0,4.5),
  lo   = c(3,4,4,3,0,3,4,4,4,3,3,1,0,6,3,
           3,5,3,4,4,3,3,3,5,2),
  hi   = c(5,4,5,5,4,4,4,5,7,4,4,4,4,6,5,
           5,5,5,7,5,4,3,4,7,6)
)

# Choose some colors
cols <- c(rainbow(30)[c(10:29,1:5)])  # rainbow colors

# horizontal barplot
par(mar=c(5,12,2,2))  # wider left margin for names
bp <- barplot(duration$mean, horiz=TRUE, names.arg=duration$Bird,
              las=1, col=cols, xlab="Days of detectable viremia", xlim=c(0,7))

# add error bars
arrows(duration$lo, bp, duration$hi, bp,
       angle=90, code=3, length=0.05, col="black", xpd=TRUE)
```

<img src="Warm-up-mosquitoes-TEMPLATE-SS_files/figure-gfm/viremia-1.png" style="display: block; margin: auto auto auto 0;" />

# STUDY QUESTION and HYPOTHESIS

## Questions

What bird species is acting as West Nile Virus amplifying host in Salt
Lake City?

## Hypothesis

House finches are acting as important amplifying hosts of West Nile
Virus in Salt Lake City.

## Prediction

Assuming house finches are acting as important amplifying hosts of West
Nile Virus in Salt Lake City, we predict that trapping locations where
mosquitoes feed on house finches will also have higher rates of
confirmed West Nile Virus in tested mosquito pools.

# METHODS

To answer the question of what bird species are amplifying West Nile
Virus trasmission in the Salt Lake area, we first set up gravid and CO2
traps around Salt Lake City to collect mosquitoes. To determine the host
species from each mosquito we are testing, we first extracted DNA from
collected mosquitoes. The DNA was collected by smashing the mosquitoes
and putting the blood that came out during the smashing into a buffer
solution. We then performed PCR on their DNA using the blood/buffer
solutions, and then sequenced multiplied DNA strands using a software
program called BLAST. BLAST software uses provided DNA codon sequences
and compared them to known sequences to identify DNA matches to
ultimately determine the closest species match for the DNA. By
performing these procedures, we are able to identify the host species
that our collected mosquitoes acquired their most recent blood meal
from. We are also able to identify patterns of where and how often the
West Nile Virus carrying mosquitoes are biting and transmitting the
virus thanks to the previously mentioned technology.

## First Analysis - Host Species of Positive and Negative Tests for WNV

This first analysis displays the number of mosquito blood meals
identified from each host species that was shown to be a match from the
DNA that was ran through the BLAST program. Each host was either
positive for West Nile Virus or negative for West Nile Virus. The bar
plot used for this comparison clearly sorts the host species into WNV
positive or negative pools and sorts the hosts from most to least blood
meal counts. The colored bar lengths make it easy to determine which
host species are most popular for mosquito blood meals in the Salt Lake
area.

``` r
## import counts_matrix: data.frame with column 'loc_positives' (0/1) and host columns 'host_*'
counts_matrix <- read.csv("./bloodmeal_plusWNV_for_BIOL3070.csv")

## 1) Identify host columns
host_cols <- grep("^host_", names(counts_matrix), value = TRUE)

if (length(host_cols) == 0) {
  stop("No columns matching '^host_' were found in counts_matrix.")
}

## 2) Ensure loc_positives is present and has both levels 0 and 1 where possible
counts_matrix$loc_positives <- factor(counts_matrix$loc_positives, levels = c(0, 1))

## 3) Aggregate host counts by loc_positives
agg <- stats::aggregate(
  counts_matrix[, host_cols, drop = FALSE],
  by = list(loc_positives = counts_matrix$loc_positives),
  FUN = function(x) sum(as.numeric(x), na.rm = TRUE)
)

## make sure both rows exist; if one is missing, add a zero row
need_levels <- setdiff(levels(counts_matrix$loc_positives), as.character(agg$loc_positives))
if (length(need_levels)) {
  zero_row <- as.list(rep(0, length(host_cols)))
  names(zero_row) <- host_cols
  for (lv in need_levels) {
    agg <- rbind(agg, c(lv, zero_row))
  }
  ## restore proper type
  agg$loc_positives <- factor(agg$loc_positives, levels = c("0","1"))
  ## coerce numeric host cols (they may have become character after rbind)
  for (hc in host_cols) agg[[hc]] <- as.numeric(agg[[hc]])
  agg <- agg[order(agg$loc_positives), , drop = FALSE]
}

## 4) Decide species order (overall abundance, descending)
overall <- colSums(agg[, host_cols, drop = FALSE], na.rm = TRUE)
host_order <- names(sort(overall, decreasing = TRUE))
species_labels <- rev(sub("^host_", "", host_order))  # nicer labels

## 5) Build count vectors for each panel in the SAME order
counts0 <- rev(as.numeric(agg[agg$loc_positives == 0, host_order, drop = TRUE]))
counts1 <- rev(as.numeric(agg[agg$loc_positives == 1, host_order, drop = TRUE]))

## 6) Colors: reuse your existing 'cols' if it exists and is long enough; otherwise generate
if (exists("cols") && length(cols) >= length(host_order)) {
  species_colors <- setNames(cols[seq_along(host_order)], species_labels)
} else {
  species_colors <- setNames(rainbow(length(host_order) + 10)[seq_along(host_order)], species_labels)
}

## 7) Shared x-limit for comparability
xmax <- max(c(counts0, counts1), na.rm = TRUE)
xmax <- if (is.finite(xmax)) xmax else 1
xlim_use <- c(0, xmax * 1.08)

## 8) Plot: two horizontal barplots with identical order and colors
op <- par(mfrow = c(1, 2),
          mar = c(4, 12, 3, 2),  # big left margin for species names
          xaxs = "i")           # a bit tighter axis padding

## Panel A: No WNV detected (loc_positives = 0)
barplot(height = counts0,
        names.arg = species_labels, 
        cex.names = .5,
        cex.axis = .5,
        col = rev(unname(species_colors[species_labels])),
        horiz = TRUE,
        las = 1,
        xlab = "Bloodmeal counts",
        main = "Locations WNV (-)",
        xlim = xlim_use)

## Panel B: WNV detected (loc_positives = 1)
barplot(height = counts1,
        names.arg = species_labels, 
        cex.names = .5,
        cex.axis = .5,
        col = rev(unname(species_colors[species_labels])),
        horiz = TRUE,
        las = 1,
        xlab = "Bloodmeal counts",
        main = "Locations WNV (+)",
        xlim = xlim_use)
```

![](Warm-up-mosquitoes-TEMPLATE-SS_files/figure-gfm/first-analysis-1.png)<!-- -->

``` r
par(op)

## Keep the colors mapping for reuse elsewhere
host_species_colors <- species_colors
```

## Second Analysis - Generalized Linear Modeling

This second analysis is a numerical and binary, generalized linear
modeling test. This test helps determine whether the amount of house
finch (the most prevalent host species for the collected mosquitoes)
blood meals is an indicator of a certain location displayed a West Nile
Virus Positive pool or had a higher rate of West Nile Virus positivity.
This statistical test analyzed both a numeric (percent positivity rate)
and a binary (positive or negative pools) aspect of West Nile Virus
presence in Salt Lake in relation to blood meal hosts. The format of
this analysis allows for a formal evaluation of the relationship
displayed in the above bar plots.

``` r
#glm with house finch alone against binary +/_
glm1 <- glm(loc_positives ~ host_House_finch,
            data = counts_matrix,
            family = binomial)
summary(glm1)
```

    ## 
    ## Call:
    ## glm(formula = loc_positives ~ host_House_finch, family = binomial, 
    ##     data = counts_matrix)
    ## 
    ## Coefficients:
    ##                  Estimate Std. Error z value Pr(>|z|)  
    ## (Intercept)       -0.1709     0.1053  -1.622   0.1047  
    ## host_House_finch   0.3468     0.1586   2.187   0.0287 *
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## (Dispersion parameter for binomial family taken to be 1)
    ## 
    ##     Null deviance: 546.67  on 394  degrees of freedom
    ## Residual deviance: 539.69  on 393  degrees of freedom
    ## AIC: 543.69
    ## 
    ## Number of Fisher Scoring iterations: 4

``` r
#glm with house-finch alone against positivity rate
glm2 <- glm(loc_rate ~ host_House_finch,
            data = counts_matrix)
summary(glm2)
```

    ## 
    ## Call:
    ## glm(formula = loc_rate ~ host_House_finch, data = counts_matrix)
    ## 
    ## Coefficients:
    ##                  Estimate Std. Error t value Pr(>|t|)    
    ## (Intercept)      0.054861   0.006755   8.122 6.07e-15 ***
    ## host_House_finch 0.027479   0.006662   4.125 4.54e-05 ***
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## (Dispersion parameter for gaussian family taken to be 0.01689032)
    ## 
    ##     Null deviance: 6.8915  on 392  degrees of freedom
    ## Residual deviance: 6.6041  on 391  degrees of freedom
    ##   (2 observations deleted due to missingness)
    ## AIC: -484.56
    ## 
    ## Number of Fisher Scoring iterations: 2

Answer: YES. Statistically significant association.

# DISCUSSION

It is especially evident in the bar plot that shows host species in
which WNV was present that house finches pose the greatest threat to
amplifying WNV in the greater Salt Lake area. House sparrows and the
American robin also pose a somewhat large threat to amplifying the
virus. It is evident that this concluded information about house finches
is significant due to the calculated p value of the data. It appears
that all other species that were identified from the blood meals of the
mosquitoes trapped did not pose a large threat to spreading WNV. One
limitation that we face when drawing these conclusions is that the
mosquitoes that are trapped represent a random sampling of the whole
mosquito population. With a relatively small percentage of mosquitoes
being able to be analyzed, the data could fail to represent blood meals
at large for mosquitoes.

## Interpretation of First Analysis

The bar plot on the left indicates trap locations where WNV is not
present and the bar plot on the right displays trap locations where WNV
is present. It can be seen from these bar plots that house finches make
up a large majority of the WNV carriers. House finches are also the most
popular blood meal among mosquitoes that are not transmitting WNV as
well.

## Interpretation of Second Analysis

From the statistical tests done for the second analysis, it can be
concluded at the data analyzed is significant. This is verified because
the p value found, 0.0287 is less than the significance level of 0.05.
There is also a positive correlation between the number of house finches
and the number of positive WNV cases, which can be identified through
the found slope.

# CONCLUSION

In conclusion, the question of what bird species acts as an amplifier
for West Nile Virus in Salt Lake City can be answered through the data
presented and the analyses conducted. It was predicted that trapping
locations where mosquitoes feed on house finches will also have higher
rates of confirmed West Nile Virus in tested mosquito pools. The data
shows that there was a positive correlation between locations with high
house finch blood meal density and positive tests for WNV. House finches
serve as the main amplifier of West Nile Virus in Salt Lake City based
off collected data.

# REFERENCES

1.  Komar N, Langevin S, Hinten S, Nemeth N, Edwards E, Hettler D, Davis
    B, Bowen R, Bunning M. Experimental infection of North American
    birds with the New York 1999 strain of West Nile virus. Emerg Infect
    Dis. 2003 Mar;9(3):311-22. <https://doi.org/10.3201/eid0903.020628>

2.  ChatGPT. OpenAI, version Jan 2025. Used as a reference for functions
    such as plot() and to correct syntax errors. Accessed 2025-11-04.

3.  West Nile virus. Utah Epidemiology. Accessed October 10, 2025.
    <https://epi.utah.gov/west-nile-virus/>.
