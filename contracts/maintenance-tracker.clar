;; Container Maintenance & Service Tracking System
;; Manages container maintenance scheduling, service providers, and maintenance history

;; Error constants
(define-constant ERR-UNAUTHORIZED (err u200))
(define-constant ERR-CONTAINER-NOT-FOUND (err u201))
(define-constant ERR-SERVICE-NOT-FOUND (err u202))
(define-constant ERR-PROVIDER-NOT-AUTHORIZED (err u203))
(define-constant ERR-MAINTENANCE-ALREADY-SCHEDULED (err u204))
(define-constant ERR-INVALID-PARAMETERS (err u205))
(define-constant ERR-SERVICE-ALREADY-COMPLETED (err u206))
(define-constant ERR-INSUFFICIENT-PAYMENT (err u207))
(define-constant ERR-MAINTENANCE-OVERDUE (err u208))

;; Contract constants
(define-constant CONTRACT-OWNER tx-sender)
(define-constant INSPECTION-INTERVAL u4320) ;; 30 days in blocks
(define-constant MAINTENANCE-INTERVAL u8640) ;; 60 days in blocks
(define-constant OVERDUE-THRESHOLD u1440) ;; 10 days grace period
(define-constant SERVICE-COMPLETION-REWARD u50000)

;; Service type constants
(define-constant SERVICE-INSPECTION "inspection")
(define-constant SERVICE-REPAIR "repair")
(define-constant SERVICE-CLEANING "cleaning")
(define-constant SERVICE-CERTIFICATION "certification")

;; Data variables
(define-data-var next-service-id uint u1)
(define-data-var next-provider-id uint u1)
(define-data-var total-maintenance-cost uint u0)

;; Maintenance service records
(define-map maintenance-services
  { service-id: uint }
  {
    container-id: uint,
    service-type: (string-ascii 20),
    provider: principal,
    scheduled-date: uint,
    actual-start: uint,
    completion-date: uint,
    service-cost: uint,
    status: (string-ascii 20),
    description: (string-ascii 200),
    urgency-level: uint,
    created-by: principal,
    created-at: uint
  }
)

;; Service provider registry
(define-map service-providers
  { provider: principal }
  {
    company-name: (string-ascii 50),
    service-types: (list 10 (string-ascii 20)),
    certification-level: uint,
    average-rating: uint,
    completed-services: uint,
    authorized: bool,
    location: (string-ascii 50),
    hourly-rate: uint,
    registration-date: uint
  }
)

;; Container maintenance schedules
(define-map maintenance-schedules
  { container-id: uint }
  {
    next-inspection: uint,
    next-maintenance: uint,
    last-service-date: uint,
    maintenance-status: (string-ascii 20),
    overdue-services: uint,
    total-service-cost: uint,
    maintenance-score: uint
  }
)

;; Service provider ratings
(define-map provider-ratings
  { service-id: uint }
  {
    rating: uint,
    review: (string-ascii 200),
    rated-by: principal,
    rated-at: uint
  }
)

;; Emergency maintenance alerts
(define-map emergency-alerts
  { container-id: uint }
  {
    alert-type: (string-ascii 30),
    severity: uint,
    reported-by: principal,
    reported-at: uint,
    resolved: bool,
    response-time: uint
  }
)

;; Register a service provider
(define-public (register-service-provider 
  (company-name (string-ascii 50))
  (service-types (list 10 (string-ascii 20)))
  (location (string-ascii 50))
  (hourly-rate uint))
  (begin
    (asserts! (is-eq tx-sender CONTRACT-OWNER) ERR-UNAUTHORIZED)
    (asserts! (> (len company-name) u2) ERR-INVALID-PARAMETERS)
    (asserts! (> hourly-rate u0) ERR-INVALID-PARAMETERS)
    
    (map-set service-providers
      { provider: tx-sender }
      {
        company-name: company-name,
        service-types: service-types,
        certification-level: u1,
        average-rating: u0,
        completed-services: u0,
        authorized: true,
        location: location,
        hourly-rate: hourly-rate,
        registration-date: stacks-block-height
      }
    )
    (ok true)
  )
)

;; Schedule maintenance service
(define-public (schedule-maintenance 
  (container-id uint)
  (service-type (string-ascii 20))
  (provider principal)
  (scheduled-date uint)
  (estimated-cost uint)
  (description (string-ascii 200))
  (urgency-level uint))
  (let 
    (
      (service-id (var-get next-service-id))
      (provider-info (unwrap! (map-get? service-providers { provider: provider }) ERR-PROVIDER-NOT-AUTHORIZED))
    )
    (asserts! (get authorized provider-info) ERR-PROVIDER-NOT-AUTHORIZED)
    (asserts! (> scheduled-date stacks-block-height) ERR-INVALID-PARAMETERS)
    (asserts! (and (>= urgency-level u1) (<= urgency-level u5)) ERR-INVALID-PARAMETERS)
    
    (map-set maintenance-services
      { service-id: service-id }
      {
        container-id: container-id,
        service-type: service-type,
        provider: provider,
        scheduled-date: scheduled-date,
        actual-start: u0,
        completion-date: u0,
        service-cost: estimated-cost,
        status: "scheduled",
        description: description,
        urgency-level: urgency-level,
        created-by: tx-sender,
        created-at: stacks-block-height
      }
    )
    
    (update-maintenance-schedule container-id service-type scheduled-date)
    (var-set next-service-id (+ service-id u1))
    (ok service-id)
  )
)

;; Start maintenance service
(define-public (start-service (service-id uint))
  (let 
    (
      (service (unwrap! (map-get? maintenance-services { service-id: service-id }) ERR-SERVICE-NOT-FOUND))
    )
    (asserts! (is-eq tx-sender (get provider service)) ERR-UNAUTHORIZED)
    (asserts! (is-eq (get status service) "scheduled") ERR-SERVICE-ALREADY-COMPLETED)
    
    (map-set maintenance-services
      { service-id: service-id }
      (merge service {
        actual-start: stacks-block-height,
        status: "in-progress"
      })
    )
    (ok true)
  )
)

;; Complete maintenance service
(define-public (complete-service (service-id uint) (actual-cost uint))
  (let 
    (
      (service (unwrap! (map-get? maintenance-services { service-id: service-id }) ERR-SERVICE-NOT-FOUND))
      (provider-info (unwrap! (map-get? service-providers { provider: (get provider service) }) ERR-PROVIDER-NOT-AUTHORIZED))
    )
    (asserts! (is-eq tx-sender (get provider service)) ERR-UNAUTHORIZED)
    (asserts! (is-eq (get status service) "in-progress") ERR-SERVICE-ALREADY-COMPLETED)
    (asserts! (> actual-cost u0) ERR-INVALID-PARAMETERS)
    
    (map-set maintenance-services
      { service-id: service-id }
      (merge service {
        completion-date: stacks-block-height,
        service-cost: actual-cost,
        status: "completed"
      })
    )
    
    ;; Update provider stats
    (map-set service-providers
      { provider: (get provider service) }
      (merge provider-info {
        completed-services: (+ (get completed-services provider-info) u1)
      })
    )
    
    ;; Update maintenance schedule
    (update-container-maintenance-record (get container-id service) actual-cost)
    
    ;; Update global cost tracking
    (var-set total-maintenance-cost (+ (var-get total-maintenance-cost) actual-cost))
    
    (ok true)
  )
)

;; Report emergency maintenance need
(define-public (report-emergency (container-id uint) (alert-type (string-ascii 30)) (severity uint))
  (begin
    (asserts! (and (>= severity u1) (<= severity u5)) ERR-INVALID-PARAMETERS)
    (asserts! (> (len alert-type) u3) ERR-INVALID-PARAMETERS)
    
    (map-set emergency-alerts
      { container-id: container-id }
      {
        alert-type: alert-type,
        severity: severity,
        reported-by: tx-sender,
        reported-at: stacks-block-height,
        resolved: false,
        response-time: u0
      }
    )
    (ok true)
  )
)

;; Rate service provider
(define-public (rate-service (service-id uint) (rating uint) (review (string-ascii 200)))
  (let 
    (
      (service (unwrap! (map-get? maintenance-services { service-id: service-id }) ERR-SERVICE-NOT-FOUND))
    )
    (asserts! (is-eq (get status service) "completed") ERR-SERVICE-ALREADY-COMPLETED)
    (asserts! (and (>= rating u1) (<= rating u5)) ERR-INVALID-PARAMETERS)
    
    (map-set provider-ratings
      { service-id: service-id }
      {
        rating: rating,
        review: review,
        rated-by: tx-sender,
        rated-at: stacks-block-height
      }
    )
    
    (update-provider-rating (get provider service) rating)
    (ok true)
  )
)

;; Update maintenance schedule for container
(define-private (update-maintenance-schedule (container-id uint) (service-type (string-ascii 20)) (scheduled-date uint))
  (let 
    (
      (schedule (default-to
        { next-inspection: u0, next-maintenance: u0, last-service-date: u0, maintenance-status: "current", overdue-services: u0, total-service-cost: u0, maintenance-score: u100 }
        (map-get? maintenance-schedules { container-id: container-id })
      ))
      (next-inspection (if (is-eq service-type SERVICE-INSPECTION) 
        (+ scheduled-date INSPECTION-INTERVAL) 
        (get next-inspection schedule)))
      (next-maintenance (if (is-eq service-type SERVICE-REPAIR) 
        (+ scheduled-date MAINTENANCE-INTERVAL) 
        (get next-maintenance schedule)))
    )
    (map-set maintenance-schedules
      { container-id: container-id }
      (merge schedule {
        next-inspection: next-inspection,
        next-maintenance: next-maintenance,
        maintenance-status: "scheduled"
      })
    )
  )
)

;; Update container maintenance record after service completion
(define-private (update-container-maintenance-record (container-id uint) (cost uint))
  (let 
    (
      (schedule (default-to
        { next-inspection: u0, next-maintenance: u0, last-service-date: u0, maintenance-status: "current", overdue-services: u0, total-service-cost: u0, maintenance-score: u100 }
        (map-get? maintenance-schedules { container-id: container-id })
      ))
    )
    (map-set maintenance-schedules
      { container-id: container-id }
      (merge schedule {
        last-service-date: stacks-block-height,
        maintenance-status: "current",
        total-service-cost: (+ (get total-service-cost schedule) cost),
        maintenance-score: (calculate-maintenance-score container-id)
      })
    )
  )
)

;; Calculate maintenance score based on service history
(define-private (calculate-maintenance-score (container-id uint))
  (let 
    (
      (schedule (map-get? maintenance-schedules { container-id: container-id }))
    )
    (match schedule
      some-schedule (let 
        (
          (overdue-count (get overdue-services some-schedule))
          (base-score u100)
        )
        (if (> overdue-count u0)
          (if (> base-score (* overdue-count u10)) (- base-score (* overdue-count u10)) u0)
          base-score
        )
      )
      u100
    )
  )
)

;; Update provider rating
(define-private (update-provider-rating (provider principal) (new-rating uint))
  (let 
    (
      (provider-info (unwrap! (map-get? service-providers { provider: provider }) false))
      (current-rating (get average-rating provider-info))
      (service-count (get completed-services provider-info))
      (updated-rating (if (> service-count u0)
        (/ (+ (* current-rating service-count) new-rating) (+ service-count u1))
        new-rating))
    )
    (map-set service-providers
      { provider: provider }
      (merge provider-info {
        average-rating: updated-rating
      })
    )
  )
)

;; Read-only functions
(define-read-only (get-maintenance-service (service-id uint))
  (map-get? maintenance-services { service-id: service-id })
)

(define-read-only (get-service-provider (provider principal))
  (map-get? service-providers { provider: provider })
)

(define-read-only (get-container-maintenance-schedule (container-id uint))
  (map-get? maintenance-schedules { container-id: container-id })
)

(define-read-only (get-service-rating (service-id uint))
  (map-get? provider-ratings { service-id: service-id })
)

(define-read-only (get-emergency-alert (container-id uint))
  (map-get? emergency-alerts { container-id: container-id })
)

(define-read-only (get-maintenance-stats)
  {
    total-services: (var-get next-service-id),
    total-cost: (var-get total-maintenance-cost),
    total-providers: (var-get next-provider-id)
  }
)

(define-read-only (is-maintenance-due (container-id uint))
  (let 
    (
      (schedule (map-get? maintenance-schedules { container-id: container-id }))
    )
    (match schedule
      some-schedule (or
        (<= (get next-inspection some-schedule) stacks-block-height)
        (<= (get next-maintenance some-schedule) stacks-block-height)
      )
      false
    )
  )
)

(define-read-only (calculate-service-cost (service-type (string-ascii 20)) (duration uint) (provider principal))
  (let 
    (
      (provider-info (map-get? service-providers { provider: provider }))
    )
    (match provider-info
      some-provider (let 
        (
          (hourly-rate (get hourly-rate some-provider))
          (base-cost (* hourly-rate duration))
          (service-multiplier (get-service-multiplier service-type))
        )
        (ok (/ (* base-cost service-multiplier) u100))
      )
      (err ERR-PROVIDER-NOT-AUTHORIZED)
    )
  )
)

;; Get service type cost multiplier
(define-private (get-service-multiplier (service-type (string-ascii 20)))
  (if (is-eq service-type SERVICE-INSPECTION)
    u100
    (if (is-eq service-type SERVICE-CLEANING)
      u80
      (if (is-eq service-type SERVICE-REPAIR)
        u150
        (if (is-eq service-type SERVICE-CERTIFICATION)
          u120
          u100
        )
      )
    )
  )
)

;; Admin functions
(define-public (authorize-provider (provider principal))
  (begin
    (asserts! (is-eq tx-sender CONTRACT-OWNER) ERR-UNAUTHORIZED)
    (let ((provider-info (unwrap! (map-get? service-providers { provider: provider }) ERR-PROVIDER-NOT-AUTHORIZED)))
      (map-set service-providers
        { provider: provider }
        (merge provider-info { authorized: true })
      )
    )
    (ok true)
  )
)

(define-public (revoke-provider (provider principal))
  (begin
    (asserts! (is-eq tx-sender CONTRACT-OWNER) ERR-UNAUTHORIZED)
    (let ((provider-info (unwrap! (map-get? service-providers { provider: provider }) ERR-PROVIDER-NOT-AUTHORIZED)))
      (map-set service-providers
        { provider: provider }
        (merge provider-info { authorized: false })
      )
    )
    (ok true)
  )
)
