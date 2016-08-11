!DEC$ IF (.NOT. DEFINED(_OPENMP))
MODULE omp_lib_kinds
 INTEGER, PARAMETER :: omp_nest_lock_kind = int_ptr_kind()
END MODULE
MODULE omp_lib
USE omp_lib_kinds
CONTAINS
 INTEGER FUNCTION omp_get_num_procs()
  omp_get_num_procs = 1
 END FUNCTION

 SUBROUTINE omp_set_num_threads(num_threads)
  INTEGER num_threads
 END SUBROUTINE

 INTEGER FUNCTION omp_get_thread_num()
  omp_get_thread_num = 0
 END FUNCTION

 INTEGER FUNCTION omp_get_num_threads()
  omp_get_num_threads = 1
 END FUNCTION

 SUBROUTINE omp_init_nest_lock(lock)
  INTEGER(kind=omp_nest_lock_kind) lock;
 END SUBROUTINE

 SUBROUTINE omp_set_nest_lock(lock)
  INTEGER(kind=omp_nest_lock_kind) lock;
 END SUBROUTINE

 SUBROUTINE omp_unset_nest_lock(lock)
  INTEGER(kind=omp_nest_lock_kind) lock;
 END SUBROUTINE

 INTEGER FUNCTION omp_test_nest_lock(lock)
  INTEGER(kind=omp_nest_lock_kind) lock;
  omp_test_nest_lock = 1
 END FUNCTION
END MODULE
!DEC$ ENDIF