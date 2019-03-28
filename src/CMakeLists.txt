cmake_minimum_required(VERSION 3.1)
project(DescObs)

option(USE_OPENMP "Use OpenMP" OFF)
option(USE_BOOST "Use Boost" OFF)
option(INTERNAL_CHECKS "Run internal consistency checks" OFF)

if(INTERNAL_CHECKS)
	add_definitions(-DINTERNAL_CHECKS)
endif()

if(USE_BOOST)
	#Boost stuff. Only used for the boost graph library.
	set(Boost_USE_STATIC_LIBS       OFF)
	set(Boost_USE_MULTITHREADED     ON)
	set(Boost_USE_STATIC_RUNTIME    OFF)
	#Find boost packages
	find_package(Boost 1.50.0 REQUIRED COMPONENTS graph regex)
	add_definitions(-DBOOST_NO_AUTO_PTR)
endif()

list(APPEND CMAKE_MODULE_PATH ${PROJECT_SOURCE_DIR}/src)
if(APPLE)
	find_library(R_LIBRARY R REQUIRED)
else()
	find_package(R REQUIRED)
endif()
find_package(Rcpp REQUIRED)
set(CMAKE_SHARED_LIBRARY_PREFIX "")
add_subdirectory(src)

add_custom_target(copyPackage ALL)
set(RFILES BootCoefQuartVar.R BootCoefVar.R CoefQuartVar.R CoefQuartVarCI.R
CoefVar.R CoefVarCI.R SampleQuantiles.R cqv_versatile.R cv_versatile.R
rm_versatile.R shortest_length.R)
#Copy package to binary directory. This works differently on windows and linux
if(WIN32)
	if("${CMAKE_GENERATOR}" STREQUAL "NMake Makefiles")
		#Copy DESCRIPTION
		add_custom_command(TARGET copyPackage COMMAND ${CMAKE_COMMAND} -E copy_if_different "${CMAKE_CURRENT_SOURCE_DIR}/DESCRIPTION.in" "${CMAKE_CURRENT_BINARY_DIR}/DESCRIPTION")
		#Copy R files
		foreach(RFILE IN LISTS RFILES)
			add_custom_command(TARGET copyPackage COMMAND ${CMAKE_COMMAND} -E copy_if_different "${CMAKE_CURRENT_SOURCE_DIR}/R/${RFILE}" "${CMAKE_CURRENT_BINARY_DIR}/R/${RFILE}")
		endforeach()
		#Run roxygen2
		add_custom_command(TARGET copyPackage COMMAND "${R_COMMAND}" ARGS "-e \"library(roxygen2);roxygen2::roxygenise()\"" WORKING_DIRECTORY "${CMAKE_CURRENT_BINARY_DIR}")
	else()
		message(STATUS "R_COMMAND = ${R_COMMAND}")
		foreach(BUILD_TYPE IN LISTS CMAKE_CONFIGURATION_TYPES)
			#Copy DESCRIPTION
			add_custom_command(TARGET copyPackage COMMAND ${CMAKE_COMMAND} -E copy_if_different "${CMAKE_CURRENT_SOURCE_DIR}/DESCRIPTION.in" "${CMAKE_CURRENT_BINARY_DIR}/${BUILD_TYPE}/DESCRIPTION")
			#Copy R files
			foreach(RFILE IN LISTS RFILES)
				add_custom_command(TARGET copyPackage COMMAND ${CMAKE_COMMAND} -E copy_if_different "${CMAKE_CURRENT_SOURCE_DIR}/R/${RFILE}" "${CMAKE_CURRENT_BINARY_DIR}/${BUILD_TYPE}/R/${RFILE}")
			endforeach()
			#Run roxygen2
			add_custom_command(TARGET copyPackage COMMAND "${R_COMMAND}" ARGS "-e" "\"library(roxygen2);roxygen2::roxygenise()\"" WORKING_DIRECTORY "${CMAKE_CURRENT_BINARY_DIR}/${BUILD_TYPE}")
		endforeach()
	endif()
else()
	#Copy DESCRIPTION
	add_custom_command(TARGET copyPackage COMMAND ${CMAKE_COMMAND} -E copy_if_different "${CMAKE_CURRENT_SOURCE_DIR}/DESCRIPTION.in" "${CMAKE_CURRENT_BINARY_DIR}/DESCRIPTION")
	#Copy R files
	foreach(RFILE IN LISTS RFILES)
		add_custom_command(TARGET copyPackage COMMAND ${CMAKE_COMMAND} -E copy_if_different "${CMAKE_CURRENT_SOURCE_DIR}/R/${RFILE}" "${CMAKE_CURRENT_BINARY_DIR}/R/${RFILE}")
	endforeach()
	#Run roxygen2
	add_custom_command(TARGET copyPackage COMMAND "${R_COMMAND}" ARGS "-e" "\"library(roxygen2);roxygen2::roxygenise()\"" WORKING_DIRECTORY "${CMAKE_CURRENT_BINARY_DIR}")
endif()
if(WIN32)
	install(CODE "execute_process(COMMAND \"${R_COMMAND}\" CMD INSTALL --no-multiarch \$\{CMAKE_INSTALL_CONFIG_NAME\} WORKING_DIRECTORY \"${PROJECT_BINARY_DIR}\")")
else()
	install(CODE "execute_process(COMMAND \"${R_COMMAND}\" CMD INSTALL . WORKING_DIRECTORY \"${PROJECT_BINARY_DIR}\")")
endif()
