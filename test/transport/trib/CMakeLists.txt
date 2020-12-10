# -------------------------------------------------------------
# file: CMakeLists.txt
# -------------------------------------------------------------
# -------------------------------------------------------------
# Battelle Memorial Institute
# Pacific Northwest Laboratory
# -------------------------------------------------------------
# -------------------------------------------------------------
# Created January 19, 2017 by William A. Perkins
# Last Change: 2020-12-10 13:31:33 d3g096
# -------------------------------------------------------------

set(SRC "${CMAKE_CURRENT_SOURCE_DIR}")

configure_file(
  "${CMAKE_CURRENT_SOURCE_DIR}/mass1.cfg.in"
  "${CMAKE_CURRENT_BINARY_DIR}/mass1.cfg"
)

configure_file(
  "${CMAKE_CURRENT_SOURCE_DIR}/link-bc-files.dat.in"
  "${CMAKE_CURRENT_BINARY_DIR}/link-bc-files.dat"
)

configure_file(
  "${CMAKE_CURRENT_SOURCE_DIR}/transport-bc-files.dat.in"
  "${CMAKE_CURRENT_BINARY_DIR}/transport-bc-files.dat"
)

set(testfiles 
  "${CMAKE_CURRENT_BINARY_DIR}/mass1.cfg"
  "${CMAKE_CURRENT_BINARY_DIR}/link-bc-files.dat"
  "${CMAKE_CURRENT_BINARY_DIR}/transport-bc-files.dat"
  "${CMAKE_CURRENT_SOURCE_DIR}/initial.dat"
  "${CMAKE_CURRENT_SOURCE_DIR}/link.dat"
  "${CMAKE_CURRENT_SOURCE_DIR}/point.dat"
  "${CMAKE_CURRENT_SOURCE_DIR}/profile_control.dat"
  "${CMAKE_CURRENT_SOURCE_DIR}/section.dat"
  "${CMAKE_CURRENT_SOURCE_DIR}/../BCFiles/inflow-bc.dat"
  "${CMAKE_CURRENT_SOURCE_DIR}/trib-bc.dat"
  "${CMAKE_CURRENT_SOURCE_DIR}/trib-conc.dat"
)

add_custom_command(
  OUTPUT "${CMAKE_CURRENT_BINARY_DIR}/profile1.out"
  COMMAND ${mass1newexe} > mass1.log
  WORKING_DIRECTORY "${CMAKE_CURRENT_BINARY_DIR}"
  COMMENT "Running test transport/trib"
  DEPENDS mass1 ${testfiles}
)
set(rundeps
  "${CMAKE_CURRENT_BINARY_DIR}/profile1.out"
  "${CMAKE_CURRENT_BINARY_DIR}/ts11.out"
  "${CMAKE_CURRENT_BINARY_DIR}/ts175.out"
  "${CMAKE_CURRENT_BINARY_DIR}/ts21.out"
  "${CMAKE_CURRENT_BINARY_DIR}/ts275.out"
  )

if (GNUPLOT_EXECUTABLE) 
  add_custom_command(
    OUTPUT plot.eps 
    COMMAND "${GNUPLOT_EXECUTABLE}" "${CMAKE_CURRENT_SOURCE_DIR}/plot.gp" > plot.eps
    WORKING_DIRECTORY "${CMAKE_CURRENT_BINARY_DIR}"
    COMMENT "Plotting test transport/trib"
    DEPENDS
    ${rundeps}
    "${CMAKE_CURRENT_BINARY_DIR}/profile1.out"
    "${CMAKE_CURRENT_BINARY_DIR}/ts11.out"
    "${CMAKE_CURRENT_BINARY_DIR}/ts175.out"
    "${CMAKE_CURRENT_BINARY_DIR}/ts21.out"
    "${CMAKE_CURRENT_BINARY_DIR}/ts275.out"
    "${CMAKE_CURRENT_SOURCE_DIR}/plot.gp"
  )
  set(rundeps plot.eps)
endif (GNUPLOT_EXECUTABLE)

add_custom_target(run_transport_trib_test
  DEPENDS ${rundeps}
)
add_dependencies(runtests run_transport_trib_test)

set_property(DIRECTORY 
  APPEND PROPERTY ADDITIONAL_MAKE_CLEAN_FILES
  debug.txt
  output.out
  profile1.out
  ts11.out ts175.out ts21.out ts275.out
  status.out
  error-warning.out
  plot.eps
  mass1.log
)

