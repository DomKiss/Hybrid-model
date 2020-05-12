library(ggplot2)


cum



ggplot(f_results_table, aes(x=test_date))+geom_line(aes(y=cum_NN), color="darkred")+geom_line(aes(y=cum_hibr), color="steelblue")+geom_line(aes(y=cum_lin), color="yellow")+facet_wrap(~ticker)

ggplot(f_results_table, aes(x=test_date, y=cum_hibr, col=ticker))+geom_line()+geom_point()

plot(f_results_table$cum_hibr)
ggplot(final_results, aes(x=colnames(final_results)))+geom_col()
