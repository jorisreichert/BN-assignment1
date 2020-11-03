library(dagitty)

g <- dagitty("dag {
    Pedu -> {famsize address internet paid activities higher}
    Pstatus -> {famsize famrel}
    age -> {goout alc}
    sex -> {alc health higher goout}
    famsize -> {freetime studytime famrel}
    address -> activities
    internet -> {activities goout higher}
    famrel -> {alc goout activities}
    freetime -> {studytime goout health}
    studytime -> {freetime health higher}
    paid -> {studytime higher}
    activities -> {studytime higher}
    goout -> {alc higher absences}
    alc -> {health higher absences}
    absences -> higher
}")

impliedConditionalIndependencies(g)

plot(g)
