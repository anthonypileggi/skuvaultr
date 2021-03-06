% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/sv_update_sale_status.R
\name{sv_update_sale_status}
\alias{sv_update_sale_status}
\title{Update the status of a sale}
\usage{
sv_update_sale_status(
  saleid,
  status = c("Completed", "Cancelled", "Pending", "ReadyToShip", "Invalid",
    "ShippedUnpaid")
)
}
\arguments{
\item{saleid}{saleID of the order (character/vector)}

\item{status}{order status to set (character/scalar)}
}
\description{
Update the status of a sale
}
