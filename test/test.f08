! w-rtf-cre-editor's automated test script
! (c) 2017 Brendyn Sonntag
! Licensed under Apache 2.0

module w_std_pipes
        ! This module handles opening a shell and executing a command.
        use, intrinsic :: iso_c_binding

        interface
                function popen(command_, type_) bind(C, name = 'popen')
                        import :: c_char, c_ptr

                        character(kind=c_char), dimension(*) :: command_
                        character(kind=c_char), dimension(*) :: type_

                        type(c_ptr) :: popen
                end function popen

                function fgets(s_, size_, stream_) bind(C, name = 'fgets')
                        import c_char, c_ptr, c_int

                        character(kind=c_char), dimension(*) :: s_
                        integer(kind=c_int), value :: size_
                        type(c_ptr), value :: stream_

                        type(c_ptr) :: fgets
                end function fgets

                function pclose(stream_) bind(C, name = 'pclose')
                        import c_ptr, c_int

                        type(c_ptr), value :: stream_

                        integer(c_int) :: pclose
                end function pclose
        end interface

        contains

        function execute_command_string(command) RESULT(str)
                character(len=*), intent(in) :: command

                type(c_ptr) :: pipe
                integer(c_int) :: swp
                character(kind=c_char,len=1000) :: line
                integer :: i

                character(len=:), allocatable :: str

                pipe = c_null_ptr
                str = ''

                pipe = popen(command//c_null_char, 'r'//c_null_char)
                if (c_associated(pipe)) then
                        do while (c_associated(fgets(line,1000,pipe)))
                                i = index(line,c_null_char)
                                if (i > 1) str = str//line(1:i-1)
                        end do
                        swp = pclose(pipe)
                end if
        end function execute_command_string

end module w_std_pipes

program w_test_wrtfe
        use w_std_pipes
        use ansicolors
        implicit none
        character(len=320) :: str
        character(len=:), allocatable :: root
        character(len=12) :: f = "example.rtf "
        integer :: i
        character(len=320), dimension(:), allocatable :: test
        character(len=320), dimension(:), allocatable :: expect

        ! Slashes matter, apparently.  So let's make them match the platform.
        call platformcolors()
        if (len(c_reset) == 0) then
                root = "..\build\wrtfe "
                print *, "WINDOWS"
        else
                root = "../build/wrtfe "
                print *, "BASH"
        end if

        allocate(test(10))
        allocate(expect(10))
        test(1) = "count"
        expect(1) = "          44"

        test(2) = "match STPH"
        expect(2) = " {\cxp. }"

        test(3) = "add WHAT what"
        expect(3) = " Added entry:"//achar(10)//"          45 WHAT -> what"

        test(4) = "del 45"
        expect(4) = " Deleted entry:"//achar(10)//"          45 WHAT -> what"

        test(5) = "findt hey"
        expect(5) = "          41 H*EU -> hey"

        test(6) = "finds H*EU"
        expect(6) = "          41 H*EU -> hey"

        test(7) = "add WHAT what"
        expect(7) = " Added entry:"//achar(10)//"          45 WHAT -> what"

        test(8) = "fixs 45 HEU"
        expect(8) = " Replaced entry:"//achar(10)&
                &//"          45 WHAT -> what"//achar(10)&
                &//"          45 HEU -> what"

        test(9) = "fixt 45 hi"
        expect(9) = " Replaced entry:"//achar(10)&
                &//"          45 HEU -> what"//achar(10)&
                &//"          45 HEU -> hi"

        test(10) = "test"
        expect(10) = " Entries 43, and           13"//achar(10)&
                &//" share translation ''{\cxp. }''"//achar(10)&
                &//" Entries 31, and           28"//achar(10)&
                &//" share translation ''{\cxp, }''"//achar(10)&
                &//" Duplicate entry detected:"//achar(10)&
                &//"          44 HEU -> hi"//achar(10)&
                &//"          45 HEU -> hi"



        ! Get version before testing.
        str = execute_command_string(root//'-v')
        print *, str

        do i=1,size(test)
                str = execute_command_string(root//f//trim(test(i)))
                if (index(str,trim(expect(i))) == 1) then
                        write (*, '(a)', advance='no') c_green//"[OK]"//c_reset
                else
                        write (*, '(a)', advance='no') c_red//"[FAILED]"//c_reset
                end if
                print *, trim(test(i))
        end do
        stop
end program w_test_wrtfe
