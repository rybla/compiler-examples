(module $skibc_module
  (rec
    (type $term (;0;) (struct (field i32) (field (ref null $term)) (field (ref null $term))))
  )
  (rec
    (type $fd_write_type (;1;) (func (param i32 i32 i32 i32) (result i32)))
  )
  (rec
    (type $main_type (;2;) (func))
  )
  (rec
    (type $print_str_type (;3;) (func (param i32 i32)))
  )
  (rec
    (type $step_type (;4;) (func (param (ref null $term)) (result (ref null $term))))
  )
  (rec
    (type $print_type (;5;) (func (param (ref null $term))))
  )
  (import "wasi_snapshot_preview1" "fd_write" (func $fd_write (;0;) (type $fd_write_type)))
  (memory $memory (;0;) 1)
  (export "memory" (memory $memory))
  (export "main" (func $main))
  (func $print_str (;1;) (type $print_str_type) (param i32 i32)
    i32.const 0
    local.get 0
    i32.store
    i32.const 4
    local.get 1
    i32.store
    i32.const 1
    i32.const 0
    i32.const 1
    i32.const 20
    call $fd_write
    drop
  )
  (func $step (;2;) (type $step_type) (param (ref null $term)) (result (ref null $term))
    (local $f (ref null $term)) (local $a (ref null $term)) (local $f_left (ref null $term)) (local $f_right (ref null $term)) (local $f_left_left (ref null $term)) (local $f_left_right (ref null $term))
    local.get 0
    ref.is_null
    if ;; label = @1
      ref.null $term
      return
    end
    local.get 0
    struct.get $term 0
    i32.const 5
    i32.ne
    if ;; label = @1
      ref.null $term
      return
    end
    local.get 0
    struct.get $term 1
    local.set $f
    local.get 0
    struct.get $term 2
    local.set $a
    local.get $f
    ref.is_null
    i32.eqz
    if ;; label = @1
      local.get $f
      struct.get $term 0
      i32.const 2
      i32.eq
      if ;; label = @2
        local.get $a
        return
      end
    end
    local.get $f
    ref.is_null
    i32.eqz
    if ;; label = @1
      local.get $f
      struct.get $term 0
      i32.const 5
      i32.eq
      if ;; label = @2
        local.get $f
        struct.get $term 1
        local.set $f_left
        local.get $f
        struct.get $term 2
        local.set $f_right
        local.get $f_left
        ref.is_null
        i32.eqz
        if ;; label = @3
          local.get $f_left
          struct.get $term 0
          i32.const 1
          i32.eq
          if ;; label = @4
            local.get $f_right
            return
          end
        end
        local.get $f_left
        ref.is_null
        i32.eqz
        if ;; label = @3
          local.get $f_left
          struct.get $term 0
          i32.const 5
          i32.eq
          if ;; label = @4
            local.get $f_left
            struct.get $term 1
            local.set $f_left_left
            local.get $f_left
            struct.get $term 2
            local.set $f_left_right
            local.get $f_left_left
            ref.is_null
            i32.eqz
            if ;; label = @5
              local.get $f_left_left
              struct.get $term 0
              i32.const 0
              i32.eq
              if ;; label = @6
                i32.const 5
                i32.const 5
                local.get $f_left_right
                local.get $a
                struct.new $term
                i32.const 5
                local.get $f_right
                local.get $a
                struct.new $term
                struct.new $term
                return
              end
              local.get $f_left_left
              struct.get $term 0
              i32.const 3
              i32.eq
              if ;; label = @6
                i32.const 5
                local.get $f_left_right
                i32.const 5
                local.get $f_right
                local.get $a
                struct.new $term
                struct.new $term
                return
              end
              local.get $f_left_left
              struct.get $term 0
              i32.const 4
              i32.eq
              if ;; label = @6
                i32.const 5
                i32.const 5
                local.get $f_left_right
                local.get $a
                struct.new $term
                local.get $f_right
                struct.new $term
                return
              end
            end
          end
        end
      end
    end
    ref.null $term
  )
  (func $deepStep (;3;) (type $step_type) (param (ref null $term)) (result (ref null $term))
    (local $res (ref null $term)) (local $f (ref null $term)) (local $a (ref null $term))
    local.get 0
    ref.is_null
    if ;; label = @1
      ref.null $term
      return
    end
    local.get 0
    struct.get $term 0
    i32.const 5
    i32.ne
    if ;; label = @1
      ref.null $term
      return
    end
    local.get 0
    struct.get $term 1
    local.set $f
    local.get 0
    struct.get $term 2
    local.set $a
    local.get 0
    call $step
    local.tee $res
    ref.is_null
    i32.eqz
    if ;; label = @1
      local.get $res
      return
    end
    local.get $a
    call $deepStep
    local.tee $res
    ref.is_null
    i32.eqz
    if ;; label = @1
      i32.const 5
      local.get $f
      local.get $res
      struct.new $term
      return
    end
    local.get $f
    call $deepStep
    local.tee $res
    ref.is_null
    i32.eqz
    if ;; label = @1
      i32.const 5
      local.get $res
      local.get $a
      struct.new $term
      return
    end
    ref.null $term
  )
  (func $evaluate (;4;) (type $step_type) (param (ref null $term)) (result (ref null $term))
    (local $current (ref null $term)) (local $next (ref null $term))
    local.get 0
    local.set $current
    loop $eval_loop
      local.get $current
      call $deepStep
      local.tee $next
      ref.is_null
      i32.eqz
      if ;; label = @2
        local.get $next
        local.set $current
        br $eval_loop
      end
    end
    local.get $current
  )
  (func $print (;5;) (type $print_type) (param (ref null $term))
    (local $f (ref null $term)) (local $a (ref null $term))
    local.get 0
    ref.is_null
    if ;; label = @1
      return
    end
    local.get 0
    struct.get $term 0
    i32.const 0
    i32.eq
    if ;; label = @1
      i32.const 100
      i32.const 1
      call $print_str
      return
    end
    local.get 0
    struct.get $term 0
    i32.const 1
    i32.eq
    if ;; label = @1
      i32.const 101
      i32.const 1
      call $print_str
      return
    end
    local.get 0
    struct.get $term 0
    i32.const 2
    i32.eq
    if ;; label = @1
      i32.const 102
      i32.const 1
      call $print_str
      return
    end
    local.get 0
    struct.get $term 0
    i32.const 3
    i32.eq
    if ;; label = @1
      i32.const 103
      i32.const 1
      call $print_str
      return
    end
    local.get 0
    struct.get $term 0
    i32.const 4
    i32.eq
    if ;; label = @1
      i32.const 104
      i32.const 1
      call $print_str
      return
    end
    local.get 0
    struct.get $term 0
    i32.const 5
    i32.eq
    if ;; label = @1
      local.get 0
      struct.get $term 1
      local.set $f
      local.get 0
      struct.get $term 2
      local.set $a
      i32.const 105
      i32.const 5
      call $print_str
      local.get $f
      struct.get $term 0
      i32.const 5
      i32.eq
      if ;; label = @2
        i32.const 111
        i32.const 1
        call $print_str
        local.get $f
        call $print
        i32.const 115
        i32.const 1
        call $print_str
      else
        local.get $f
        call $print
      end
      i32.const 114
      i32.const 1
      call $print_str
      local.get $a
      struct.get $term 0
      i32.const 5
      i32.eq
      if ;; label = @2
        i32.const 111
        i32.const 1
        call $print_str
        local.get $a
        call $print
        i32.const 115
        i32.const 1
        call $print_str
      else
        local.get $a
        call $print
      end
    end
  )
  (func $main (;6;) (type $main_type)
    i32.const 5
    i32.const 5
    i32.const 5
    i32.const 0
    ref.null $term
    ref.null $term
    struct.new $term
    i32.const 3
    ref.null $term
    ref.null $term
    struct.new $term
    struct.new $term
    i32.const 2
    ref.null $term
    ref.null $term
    struct.new $term
    struct.new $term
    i32.const 2
    ref.null $term
    ref.null $term
    struct.new $term
    struct.new $term
    call $evaluate
    call $print
    i32.const 116
    i32.const 1
    call $print_str
  )
  (data (;0;) (i32.const 100) "S")
  (data (;1;) (i32.const 101) "K")
  (data (;2;) (i32.const 102) "I")
  (data (;3;) (i32.const 103) "B")
  (data (;4;) (i32.const 104) "C")
  (data (;5;) (i32.const 105) "App1 ")
  (data (;6;) (i32.const 111) "(")
  (data (;7;) (i32.const 114) " ")
  (data (;8;) (i32.const 115) ")")
  (data (;9;) (i32.const 116) "\0a")
)
