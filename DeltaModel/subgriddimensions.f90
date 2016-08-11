INTEGER, PARAMETER:: timeSpaceFactor=1
INTEGER, PARAMETER:: cols=140/timeSpaceFactor, rows=140/timeSpaceFactor
INTEGER :: cellsize = 2*timeSpaceFactor     ! size of one side of ea square cell (m)
INTEGER:: timeOffset=timeSpaceFactor
!INTEGER, PARAMETER :: cols = subgrid(2)-subgrid(1)+1 ! total number of columns in sub-grid
!INTEGER, PARAMETER :: rows = subgrid(4)-subgrid(3)+1 ! total number of rows in sub-grid