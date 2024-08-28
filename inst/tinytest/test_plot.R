library("shiny")
using("tinysnapshot")

plot_out <- kinesis:::output_plot("x", title = "Un titre", note = "Une note")
expect_snapshot_print(as.character(plot_out), label = "output_plot")

par_cex <- kinesis:::select_cex("x")
expect_snapshot_print(as.character(par_cex), label = "select_cex")

par_pch <- kinesis:::select_pch("x")
expect_snapshot_print(as.character(par_pch), label = "select_pch")

par_lty <- kinesis:::select_lty("x")
expect_snapshot_print(as.character(par_lty), label = "select_lty")

par_col <- kinesis:::select_color("x")
expect_snapshot_print(as.character(par_col), label = "select_col")
