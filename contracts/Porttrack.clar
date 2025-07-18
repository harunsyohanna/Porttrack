(define-constant CONTRACT_OWNER tx-sender)
(define-constant ERR_UNAUTHORIZED (err u100))
(define-constant ERR_CONTAINER_NOT_FOUND (err u101))
(define-constant ERR_CONTAINER_EXISTS (err u102))
(define-constant ERR_INVALID_STATUS (err u103))
(define-constant ERR_INVALID_PORT (err u104))
(define-constant ERR_SAME_PORT (err u105))
(define-constant ERR_POLICY_NOT_FOUND (err u106))
(define-constant ERR_POLICY_EXPIRED (err u107))
(define-constant ERR_INSUFFICIENT_PAYMENT (err u108))
(define-constant ERR_CLAIM_NOT_FOUND (err u109))
(define-constant ERR_CLAIM_ALREADY_PROCESSED (err u110))
(define-constant ERR_INVALID_CLAIM_AMOUNT (err u111))
(define-constant ERR_PROVIDER_NOT_AUTHORIZED (err u112))
(define-constant ERR_POLICY_ALREADY_EXISTS (err u113))

(define-data-var next-container-id uint u1)
(define-data-var next-policy-id uint u1)
(define-data-var next-claim-id uint u1)

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

(define-map insurance-policies
  { policy-id: uint }
  {
    container-id: uint,
    provider: principal,
    policyholder: principal,
    coverage-amount: uint,
    premium-amount: uint,
    premium-paid: uint,
    start-block: uint,
    end-block: uint,
    status: (string-ascii 20),
    cargo-type: (string-ascii 50),
    created-at: uint
  }
)

(define-map insurance-claims
  { claim-id: uint }
  {
    policy-id: uint,
    container-id: uint,
    claimant: principal,
    claim-amount: uint,
    claim-reason: (string-ascii 100),
    evidence-hash: (string-ascii 64),
    status: (string-ascii 20),
    filed-at: uint,
    processed-at: uint,
    processed-by: principal,
    settlement-amount: uint
  }
)

(define-map insurance-providers
  { provider: principal }
  { 
    authorized: bool,
    company-name: (string-ascii 50),
    license-number: (string-ascii 30)
  }
)

(define-map policy-premiums
  { cargo-type: (string-ascii 50) }
  { 
    base-rate: uint,
    risk-multiplier: uint
  }
)

(define-map container-policies
  { container-id: uint }
  { policy-id: uint }
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

(define-public (authorize-insurance-provider 
  (provider principal)
  (company-name (string-ascii 50))
  (license-number (string-ascii 30)))
  (begin
    (asserts! (is-eq tx-sender CONTRACT_OWNER) ERR_UNAUTHORIZED)
    (map-set insurance-providers 
      { provider: provider }
      {
        authorized: true,
        company-name: company-name,
        license-number: license-number
      }
    )
    (ok true)
  )
)

(define-public (revoke-insurance-provider (provider principal))
  (begin
    (asserts! (is-eq tx-sender CONTRACT_OWNER) ERR_UNAUTHORIZED)
    (map-set insurance-providers 
      { provider: provider }
      {
        authorized: false,
        company-name: "",
        license-number: ""
      }
    )
    (ok true)
  )
)

(define-public (set-premium-rate 
  (cargo-type (string-ascii 50))
  (base-rate uint)
  (risk-multiplier uint))
  (begin
    (asserts! (is-eq tx-sender CONTRACT_OWNER) ERR_UNAUTHORIZED)
    (map-set policy-premiums
      { cargo-type: cargo-type }
      {
        base-rate: base-rate,
        risk-multiplier: risk-multiplier
      }
    )
    (ok true)
  )
)

(define-public (create-insurance-policy
  (container-id uint)
  (coverage-amount uint)
  (duration-blocks uint)
  (cargo-type (string-ascii 50)))
  (let (
    (policy-id (var-get next-policy-id))
    (container (unwrap! (map-get? containers { container-id: container-id }) ERR_CONTAINER_NOT_FOUND))
    (premium-info (unwrap! (map-get? policy-premiums { cargo-type: cargo-type }) ERR_INVALID_CLAIM_AMOUNT))
    (premium-amount (calculate-premium coverage-amount duration-blocks (get base-rate premium-info) (get risk-multiplier premium-info)))
  )
    (asserts! (is-authorized-provider tx-sender) ERR_PROVIDER_NOT_AUTHORIZED)
    (asserts! (is-none (map-get? container-policies { container-id: container-id })) ERR_POLICY_ALREADY_EXISTS)
    (asserts! (> coverage-amount u0) ERR_INVALID_CLAIM_AMOUNT)
    (asserts! (> duration-blocks u0) ERR_INVALID_CLAIM_AMOUNT)
    
    (map-set insurance-policies
      { policy-id: policy-id }
      {
        container-id: container-id,
        provider: tx-sender,
        policyholder: (get owner container),
        coverage-amount: coverage-amount,
        premium-amount: premium-amount,
        premium-paid: u0,
        start-block: stacks-block-height,
        end-block: (+ stacks-block-height duration-blocks),
        status: "pending-payment",
        cargo-type: cargo-type,
        created-at: stacks-block-height
      }
    )
    
    (map-set container-policies
      { container-id: container-id }
      { policy-id: policy-id }
    )
    
    (var-set next-policy-id (+ policy-id u1))
    (ok policy-id)
  )
)

(define-public (pay-premium (policy-id uint) (payment-amount uint))
  (let (
    (policy (unwrap! (map-get? insurance-policies { policy-id: policy-id }) ERR_POLICY_NOT_FOUND))
    (remaining-premium (- (get premium-amount policy) (get premium-paid policy)))
  )
    (asserts! (is-eq tx-sender (get policyholder policy)) ERR_UNAUTHORIZED)
    (asserts! (>= payment-amount remaining-premium) ERR_INSUFFICIENT_PAYMENT)
    (asserts! (is-eq (get status policy) "pending-payment") ERR_CLAIM_ALREADY_PROCESSED)
    
    (map-set insurance-policies
      { policy-id: policy-id }
      (merge policy {
        premium-paid: (get premium-amount policy),
        status: "active"
      })
    )
    
    (ok true)
  )
)

(define-public (file-insurance-claim
  (policy-id uint)
  (claim-amount uint)
  (claim-reason (string-ascii 100))
  (evidence-hash (string-ascii 64)))
  (let (
    (claim-id (var-get next-claim-id))
    (policy (unwrap! (map-get? insurance-policies { policy-id: policy-id }) ERR_POLICY_NOT_FOUND))
  )
    (asserts! (is-eq tx-sender (get policyholder policy)) ERR_UNAUTHORIZED)
    (asserts! (is-eq (get status policy) "active") ERR_POLICY_EXPIRED)
    (asserts! (<= stacks-block-height (get end-block policy)) ERR_POLICY_EXPIRED)
    (asserts! (<= claim-amount (get coverage-amount policy)) ERR_INVALID_CLAIM_AMOUNT)
    (asserts! (> claim-amount u0) ERR_INVALID_CLAIM_AMOUNT)
    
    (map-set insurance-claims
      { claim-id: claim-id }
      {
        policy-id: policy-id,
        container-id: (get container-id policy),
        claimant: tx-sender,
        claim-amount: claim-amount,
        claim-reason: claim-reason,
        evidence-hash: evidence-hash,
        status: "pending",
        filed-at: stacks-block-height,
        processed-at: u0,
        processed-by: (get provider policy),
        settlement-amount: u0
      }
    )
    
    (var-set next-claim-id (+ claim-id u1))
    (ok claim-id)
  )
)

(define-public (process-insurance-claim
  (claim-id uint)
  (approve bool)
  (settlement-amount uint))
  (let (
    (claim (unwrap! (map-get? insurance-claims { claim-id: claim-id }) ERR_CLAIM_NOT_FOUND))
    (policy (unwrap! (map-get? insurance-policies { policy-id: (get policy-id claim) }) ERR_POLICY_NOT_FOUND))
  )
    (asserts! (is-eq tx-sender (get provider policy)) ERR_UNAUTHORIZED)
    (asserts! (is-eq (get status claim) "pending") ERR_CLAIM_ALREADY_PROCESSED)
    (asserts! (or (not approve) (<= settlement-amount (get claim-amount claim))) ERR_INVALID_CLAIM_AMOUNT)
    
    (map-set insurance-claims
      { claim-id: claim-id }
      (merge claim {
        status: (if approve "approved" "denied"),
        processed-at: stacks-block-height,
        settlement-amount: (if approve settlement-amount u0)
      })
    )
    
    (ok true)
  )
)

(define-public (cancel-policy (policy-id uint))
  (let (
    (policy (unwrap! (map-get? insurance-policies { policy-id: policy-id }) ERR_POLICY_NOT_FOUND))
  )
    (asserts! (or 
      (is-eq tx-sender (get policyholder policy))
      (is-eq tx-sender (get provider policy))
    ) ERR_UNAUTHORIZED)
    (asserts! (not (is-eq (get status policy) "cancelled")) ERR_CLAIM_ALREADY_PROCESSED)
    
    (map-set insurance-policies
      { policy-id: policy-id }
      (merge policy { status: "cancelled" })
    )
    
    (ok true)
  )
)

(define-read-only (get-insurance-policy (policy-id uint))
  (map-get? insurance-policies { policy-id: policy-id })
)

(define-read-only (get-insurance-claim (claim-id uint))
  (map-get? insurance-claims { claim-id: claim-id })
)

(define-read-only (get-container-policy (container-id uint))
  (map-get? container-policies { container-id: container-id })
)

(define-read-only (get-provider-info (provider principal))
  (map-get? insurance-providers { provider: provider })
)

(define-read-only (get-premium-rate (cargo-type (string-ascii 50)))
  (map-get? policy-premiums { cargo-type: cargo-type })
)

(define-read-only (is-authorized-provider (provider principal))
  (default-to false (get authorized (map-get? insurance-providers { provider: provider })))
)

(define-read-only (get-next-policy-id)
  (var-get next-policy-id)
)

(define-read-only (get-next-claim-id)
  (var-get next-claim-id)
)

(define-private (calculate-premium 
  (coverage-amount uint)
  (duration-blocks uint)
  (base-rate uint)
  (risk-multiplier uint))
  (let (
    (base-premium (/ (* coverage-amount base-rate) u10000))
    (duration-factor (/ duration-blocks u1000))
    (risk-adjusted (/ (* base-premium risk-multiplier) u100))
  )
    (* risk-adjusted duration-factor)
  )
)