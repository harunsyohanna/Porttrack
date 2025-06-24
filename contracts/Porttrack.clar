(define-constant CONTRACT_OWNER tx-sender)
(define-constant ERR_UNAUTHORIZED (err u100))
(define-constant ERR_CONTAINER_NOT_FOUND (err u101))
(define-constant ERR_CONTAINER_EXISTS (err u102))
(define-constant ERR_INVALID_STATUS (err u103))
(define-constant ERR_INVALID_PORT (err u104))
(define-constant ERR_SAME_PORT (err u105))

(define-data-var next-container-id uint u1)

(define-map containers
  { container-id: uint }
  {
    owner: principal,
    container-number: (string-ascii 20),
    current-port: (string-ascii 50),
    destination-port: (string-ascii 50),
    status: (string-ascii 20),
    cargo-description: (string-ascii 100),
    weight: uint,
    created-at: uint,
    last-updated: uint
  }
)

(define-map container-history
  { container-id: uint, sequence: uint }
  {
    port: (string-ascii 50),
    status: (string-ascii 20),
    timestamp: uint,
    updated-by: principal
  }
)

(define-map container-sequence
  { container-id: uint }
  { next-sequence: uint }
)

(define-map port-containers
  { port: (string-ascii 50) }
  { container-count: uint }
)

(define-map authorized-ports
  { port: (string-ascii 50) }
  { authorized: bool }
)

(define-map port-operators
  { operator: principal }
  { authorized: bool }
)

(define-public (register-container 
  (container-number (string-ascii 20))
  (current-port (string-ascii 50))
  (destination-port (string-ascii 50))
  (cargo-description (string-ascii 100))
  (weight uint))
  (let ((container-id (var-get next-container-id)))
    (asserts! (is-authorized-operator tx-sender) ERR_UNAUTHORIZED)
    (asserts! (is-port-authorized current-port) ERR_INVALID_PORT)
    (asserts! (is-port-authorized destination-port) ERR_INVALID_PORT)
    (asserts! (not (is-eq current-port destination-port)) ERR_SAME_PORT)
    (map-set containers
      { container-id: container-id }
      {
        owner: tx-sender,
        container-number: container-number,
        current-port: current-port,
        destination-port: destination-port,
        status: "registered",
        cargo-description: cargo-description,
        weight: weight,
        created-at: stacks-block-height,
        last-updated: stacks-block-height
      }
    )
    (map-set container-history
      { container-id: container-id, sequence: u0 }
      {
        port: current-port,
        status: "registered",
        timestamp: stacks-block-height,
        updated-by: tx-sender
      }
    )
    (map-set container-sequence
      { container-id: container-id }
      { next-sequence: u1 }
    )
    (update-port-count current-port true)
    (var-set next-container-id (+ container-id u1))
    (ok container-id)
  )
)

(define-public (update-container-status
  (container-id uint)
  (new-port (string-ascii 50))
  (new-status (string-ascii 20)))
  (let (
    (container (unwrap! (map-get? containers { container-id: container-id }) ERR_CONTAINER_NOT_FOUND))
    (current-sequence (default-to u0 (get next-sequence (map-get? container-sequence { container-id: container-id }))))
  )
    (asserts! (is-authorized-operator tx-sender) ERR_UNAUTHORIZED)
    (asserts! (is-port-authorized new-port) ERR_INVALID_PORT)
    (asserts! (is-valid-status new-status) ERR_INVALID_STATUS)
    
    (if (not (is-eq (get current-port container) new-port))
      (begin
        (update-port-count (get current-port container) false)
        (update-port-count new-port true)
      )
      true
    )
    
    (map-set containers
      { container-id: container-id }
      (merge container {
        current-port: new-port,
        status: new-status,
        last-updated: stacks-block-height
      })
    )
    
    (map-set container-history
      { container-id: container-id, sequence: current-sequence }
      {
        port: new-port,
        status: new-status,
        timestamp: stacks-block-height,
        updated-by: tx-sender
      }
    )
    
    (map-set container-sequence
      { container-id: container-id }
      { next-sequence: (+ current-sequence u1) }
    )
    
    (ok true)
  )
)

(define-public (authorize-port (port (string-ascii 50)))
  (begin
    (asserts! (is-eq tx-sender CONTRACT_OWNER) ERR_UNAUTHORIZED)
    (map-set authorized-ports { port: port } { authorized: true })
    (ok true)
  )
)

(define-public (revoke-port (port (string-ascii 50)))
  (begin
    (asserts! (is-eq tx-sender CONTRACT_OWNER) ERR_UNAUTHORIZED)
    (map-set authorized-ports { port: port } { authorized: false })
    (ok true)
  )
)

(define-public (authorize-operator (operator principal))
  (begin
    (asserts! (is-eq tx-sender CONTRACT_OWNER) ERR_UNAUTHORIZED)
    (map-set port-operators { operator: operator } { authorized: true })
    (ok true)
  )
)

(define-public (revoke-operator (operator principal))
  (begin
    (asserts! (is-eq tx-sender CONTRACT_OWNER) ERR_UNAUTHORIZED)
    (map-set port-operators { operator: operator } { authorized: false })
    (ok true)
  )
)

(define-read-only (get-container (container-id uint))
  (map-get? containers { container-id: container-id })
)

(define-read-only (get-container-history (container-id uint) (sequence uint))
  (map-get? container-history { container-id: container-id, sequence: sequence })
)

(define-read-only (get-port-container-count (port (string-ascii 50)))
  (default-to u0 (get container-count (map-get? port-containers { port: port })))
)

(define-read-only (is-port-authorized (port (string-ascii 50)))
  (default-to false (get authorized (map-get? authorized-ports { port: port })))
)

(define-read-only (is-authorized-operator (operator principal))
  (or 
    (is-eq operator CONTRACT_OWNER)
    (default-to false (get authorized (map-get? port-operators { operator: operator })))
  )
)

(define-read-only (get-next-container-id)
  (var-get next-container-id)
)

(define-read-only (get-container-sequence (container-id uint))
  (map-get? container-sequence { container-id: container-id })
)

(define-private (is-valid-status (status (string-ascii 20)))
  (or
    (is-eq status "registered")
    (or
      (is-eq status "in-transit")
      (or
        (is-eq status "arrived")
        (or
          (is-eq status "loading")
          (or
            (is-eq status "unloading")
            (or
              (is-eq status "customs")
              (or
                (is-eq status "released")
                (is-eq status "delivered")
              )
            )
          )
        )
      )
    )
  )
)

(define-private (update-port-count (port (string-ascii 50)) (increment bool))
  (let ((current-count (get-port-container-count port)))
    (map-set port-containers
      { port: port }
      { 
        container-count: (if increment 
          (+ current-count u1) 
          (if (> current-count u0) (- current-count u1) u0)
        )
      }
    )
  )
)