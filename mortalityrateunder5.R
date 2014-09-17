library(WDI)
library(rMaps)
library(dplyr)
library(countrycode)
library(RColorBrewer)
library(plyr)
library(rCharts)

df=WDI(country = "all", indicator = "SH.DYN.MORT",
    start = 1970, end = 2013, extra = FALSE, cache = NULL)
data <- df %.% 
  na.omit() %.%
  #Add iso3c format country code 
  mutate(iso3c=countrycode(iso2c, "iso2c", "iso3c")) %.% 
  group_by(iso3c) 


i1 <- ichoropleth(SH.DYN.MORT~iso3c, data, map="world",labels=FALSE,pal="Reds",animate='year', play=TRUE)
i1$publish("infantdeathsall",host="gist")
# http://bl.ocks.org/patilv/raw/410a1de459998f35599a/


# Gender specific

dfgender=WDI(country = "all", indicator = c("SH.DYN.MORT.FE","SH.DYN.MORT.MA"),
       start = 1970, end = 2013, extra = FALSE, cache = NULL)

dfgender$femaleslessmales=dfgender$SH.DYN.MORT.FE-dfgender$SH.DYN.MORT.MA

data <- dfgender %.% 
  na.omit() %.%
  #Add iso3c format country code 
  mutate(iso3c=countrycode(iso2c, "iso2c", "iso3c")) %.% 
  group_by(iso3c) 


datasub=data[data$femaleslessmales<=0,]
datasub1 = transform(datasub, 
                 fillKey = cut(
                   datasub$femaleslessmales, 
                   quantile(datasub$femaleslessmales, seq(0, 1, 1/5)),
                   ordered_result = TRUE
                 )
)

datasub2=data[data$femaleslessmales>0,]
datasub2 = transform(datasub2, 
                     fillKey = cut(
                       datasub2$femaleslessmales, 
                       quantile(datasub2$femaleslessmales, seq(0, 1, 1/1)),
                       ordered_result = TRUE
                     )
)
data=rbind(datasub1,datasub2)

Datamaps = setRefClass('Datamaps', contains = 'rCharts', methods = list(
  initialize = function(){
    callSuper();
    LIB <<- get_lib(system.file('libraries', 'datamaps', package = 'rMaps'))
  },
  getPayload = function(chartId){
    params_ = params[!(names(params) %in% "popup_template")]
    list(
      chartParams = toJSON2(params_), 
      chartId = chartId, lib = basename(lib),
      popup_template = params$popup_template
    )
  }  
))



ichoroplethcustom <- function (x, data, pal = "Blues", ncuts = 6, animate = NULL, 
                               play = T, map = "usa", legend = TRUE, labels = TRUE, ...) 
{
  d <- Datamaps$new()
  fml = lattice::latticeParseFormula(x, data = data)
  fillColors = brewer.pal(ncuts, pal)
  d$set(scope = map, fills = as.list(setNames(fillColors, levels(data$fillKey))), 
        legend = legend, labels = labels, ...)
  if (!is.null(animate)) {
    range_ = summary(data[[animate]])
    data = dlply(data, animate, function(x) {
      y = toJSONArray2(x, json = F)
      names(y) = lapply(y, "[[", fml$right.name)
      return(y)
    })
    d$set(bodyattrs = "ng-app ng-controller='rChartsCtrl'")
    d$addAssets(jshead = "http://cdnjs.cloudflare.com/ajax/libs/angular.js/1.2.1/angular.min.js")
    if (play == T) {
      d$setTemplate(chartDiv = sprintf("\n        <div class='container'>\n         <button ng-click='animateMap()'>Play</button>\n         <span ng-bind='year'></span>\n         <div id='{{chartId}}' class='rChart datamaps'></div>\n        </div>\n        <script>\n          function rChartsCtrl($scope, $timeout){\n            $scope.year = %s;\n              $scope.animateMap = function(){\n              if ($scope.year > %s){\n                return;\n              }\n              map{{chartId}}.updateChoropleth(chartParams.newData[$scope.year]);\n              $scope.year += 1\n              $timeout($scope.animateMap, 1000)\n            }\n          }\n       </script>", 
                                       range_[1], range_[6]))
    }
    else {
      d$setTemplate(chartDiv = sprintf("\n        <div class='container'>\n          <input id='slider' type='range' min=%s max=%s ng-model='year' width=200>\n          <span ng-bind='year'></span>\n          <div id='{{chartId}}' class='rChart datamaps'></div>          \n        </div>\n        <script>\n          function rChartsCtrl($scope){\n            $scope.year = %s;\n            $scope.$watch('year', function(newYear){\n              map{{chartId}}.updateChoropleth(chartParams.newData[newYear]);\n            })\n          }\n       </script>", 
                                       range_[1], range_[6], range_[1]))
    }
    d$set(newData = data, data = data[[1]])
  }
  else {
    d$set(data = dlply(data, fml$right.name))
  }
  return(d)
}


i2 <- ichoroplethcustom(femaleslessmales~iso3c, data, map="world",labels=FALSE,pal="Paired",animate='year', play=T)
i2
i2$publish("femaleslessmales",host="gist")

#http://bl.ocks.org/patilv/raw/a0311c4a4d8719d487f9/

