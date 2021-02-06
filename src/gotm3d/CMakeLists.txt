add_library(gotm3d
   gotm3d.F90
   ncio.F90
   print_version_3d.F90
)
target_link_libraries(gotm3d PUBLIC gotm util config)
set_property(TARGET gotm3d PROPERTY FOLDER gotm3d)
