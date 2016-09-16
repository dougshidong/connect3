module ai

    use prec
    use globals
    use boardclass 
    implicit none

    contains

    recursive subroutine alphabeta( &
        v, node, ialpha, ibeta, iplayer, depth, maxdepth, bestmove)

        implicit none

        type(board)  :: node
        integer(pi)  :: iplayer
        integer(pi4) :: ialpha, ibeta 
        integer(pi4) :: v, vab, depth, alpha, beta, maxdepth
        integer(pi)  :: player, ichild, nchildren
        type(act)    :: acts(16), bestmove
        integer(pi)  :: ipiece

        alpha = ialpha
        beta = ibeta
        player = iplayer

        v = 0
        vab = 0
        call checkwinner(node)
        if(node%winner.ne.0) then
!           Substracting node depth encourages the fewest winning moves
!           Without depth, it may 'taunt' the enemy with an uppper hand forever
            v = node%winner * ((big-1) - node%depth)
            return
        end if
        
        if(depth.eq.maxdepth) then
            v = evalHeuristic(node)
            return
        end if

!       Maximizing player
        if(player.eq.p1) then
            v = -big
            call generateActions(node, p1, nchildren, acts)
            allocate(node%children(nchildren))
            do ichild = 1, nchildren
                allocate(node%children(ichild)%squares(imax, jmax))
!               node%children(ichild)%squares = node%squares
!               node%children(ichild)%pieces  = node%pieces
!               node%children(ichild)%winner  = node%winner
!               node%children(ichild)%depth   = node%depth + 1
!               node%children(ichild)%turn    = p2
!               call movepiece(node%children(ichild), acts(ichild))
                call makechild(node, node%children(ichild), acts(ichild))

                vab = 0
                call alphabeta(vab, node%children(ichild), &
                        alpha, beta, p2, depth + 1, maxdepth, bestmove)

                if(depth.eq.0) then
                    if(vab.gt.v) then 
                        bestmove = acts(ichild)
                    endif
                end if

                v = max(v, vab)
                if(depth.eq.0) then
                    write(*,'(I2,"  ",2I1,A1," ",I0)') & 
                    ichild, acts(ichild)%i, acts(ichild)%j, acts(ichild)%d, v
                end if
                alpha = max(alpha, v)
                

                if(beta.le.alpha) exit
            end do

            do ichild = 1, nchildren
                if(allocated(node%children(ichild)%squares)) then
                    deallocate(node%children(ichild)%squares)
                end if
            end do

            deallocate(node%children)

            return
!       Minimizing player
        else
            v = big
            call generateActions(node, p2, nchildren, acts)
            allocate(node%children(nchildren))
            do ichild = 1, nchildren
                allocate(node%children(ichild)%squares(imax, jmax))
!               node%children(ichild)%squares = node%squares
!               node%children(ichild)%pieces  = node%pieces
!               node%children(ichild)%winner  = node%winner
!               node%children(ichild)%depth   = node%depth + 1
!               node%children(ichild)%turn    = p1
!               call movepiece(node%children(ichild), acts(ichild))
                call makechild(node, node%children(ichild), acts(ichild))

                vab = 0
                call alphabeta(vab, node%children(ichild), &
                        alpha, beta, p1, depth + 1, maxdepth, bestmove)
                if(depth.eq.0) then
                    if(vab.lt.v) then 
                        bestmove = acts(ichild)
                    endif
                end if

                v = min(v, vab)
                if(depth.eq.0) then
                    write(*,'(I2,"  ",2I1,A1," ",I0)') & 
                    ichild, acts(ichild)%i, acts(ichild)%j, acts(ichild)%d, v
                end if

                beta = min(beta, v)
                if(beta.le.alpha) exit
            end do

            do ichild = 1, nchildren
                if(allocated(node%children(ichild)%squares)) then
                    deallocate(node%children(ichild)%squares)
                end if
            end do

            deallocate(node%children)

            return
        end if


        return

    end subroutine

    function evalHeuristic(state) result(h)
        implicit none
        type (board) :: state
        integer(pi4) :: h

        h = h2run(state)

        return

    end function

    function h2run(state) result(h)
        implicit none
        type (board) :: state
        integer(pi4) :: h
        integer(pi)  :: i, j
        integer(pi)  :: xi, yi, xj, yj

        h = 0

        do i = 1, 3
        do j = i+one, 4
            xi = state%pieces(1,i,1)
            yi = state%pieces(2,i,1)

            xj = state%pieces(1,j,1)
            yj = state%pieces(2,j,1)

            if(abs(xi - xj).eq.1 .and. abs(yi - yj).eq.1) then
                h = h + 7
            end if

            xi = state%pieces(1,i,2)
            yi = state%pieces(2,i,2)

            xj = state%pieces(1,j,2)
            yj = state%pieces(2,j,2)

            if(abs(xi - xj).eq.1 .and. abs(yi - yj).eq.1) then
                h = h - 7
            end if
        end do
        end do

        if(state%turn.eq.p1) h = h - state%depth
        if(state%turn.eq.p2) h = h + state%depth

        return

    end function

    subroutine generateActions(state, player, nmove, moves)
    implicit none
    type(board) :: state
    integer(pi) :: player, ip, ipiece, x, y, p, nmove
    type(act)   :: moves(16)

    nmove = 0
    if(player.eq.p1) then
        ip = 1
    else
        ip = 2
    end if

    do ipiece = 1, 4
        x = state%pieces(1,ipiece,ip)
        y = state%pieces(2,ipiece,ip)

        if(x.ne.imax) then
            p = state%squares(x+1,y)
            if(p.eq.empty) then
                nmove = nmove + one
                moves(nmove) % i = x
                moves(nmove) % j = y
                moves(nmove) % d = 'E'
            end if
        end if
        if(y.ne.jmax) then
            p = state%squares(x,y+1)
            if(p.eq.empty) then
                nmove = nmove + one
                moves(nmove) % i = x
                moves(nmove) % j = y
                moves(nmove) % d = 'S'
            end if
        end if
        if(x.ne.1) then
            p = state%squares(x-1,y)
            if(p.eq.empty) then
                nmove = nmove + one
                moves(nmove) % i = x
                moves(nmove) % j = y
                moves(nmove) % d = 'W'
            end if
        end if
        if(y.ne.1) then
            p = state%squares(x,y-1)
            if(p.eq.empty) then
                nmove = nmove + one
                moves(nmove) % i = x
                moves(nmove) % j = y
                moves(nmove) % d = 'N'
            end if
        end if
        
    end do
        
    end subroutine


end module ai
