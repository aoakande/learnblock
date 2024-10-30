;; BlockLearnAdemy Course Management Contract

;; Constants
(define-constant contract-owner tx-sender)
(define-constant err-owner-only (err u100))
(define-constant err-not-found (err u101))
(define-constant err-already-exists (err u102))

;; Data Variables
(define-data-var next-course-id uint u1)

;; Define the course struct
(define-map courses
  { course-id: uint }
  {
    title: (string-utf8 100),
    description: (string-utf8 500),
    instructor: principal,
    price: uint,
    total-shares: uint,
    available-shares: uint
  }
)

;; Define ownership mapping
(define-map course-ownership
  { course-id: uint, owner: principal }
  { shares: uint }
)

;; Read-only functions

(define-read-only (get-course (course-id uint))
  (map-get? courses { course-id: course-id })
)

(define-read-only (get-course-ownership (course-id uint) (owner principal))
  (map-get? course-ownership { course-id: course-id, owner: owner })
)

;; Public functions

(define-public (create-course (title (string-utf8 100)) (description (string-utf8 500)) (price uint) (total-shares uint))
  (let
    (
      (course-id (var-get next-course-id))
    )
    (asserts! (is-eq tx-sender contract-owner) err-owner-only)
    (asserts! (is-none (map-get? courses { course-id: course-id })) err-already-exists)
    (map-set courses
      { course-id: course-id }
      {
        title: title,
        description: description,
        instructor: tx-sender,
        price: price,
        total-shares: total-shares,
        available-shares: total-shares
      }
    )
    (map-set course-ownership
      { course-id: course-id, owner: tx-sender }
      { shares: total-shares }
    )
    (var-set next-course-id (+ course-id u1))
    (ok course-id)
  )
)

(define-public (update-course (course-id uint) (title (string-utf8 100)) (description (string-utf8 500)) (price uint))
  (let
    (
      (course (unwrap! (map-get? courses { course-id: course-id }) err-not-found))
    )
    (asserts! (is-eq (get instructor course) tx-sender) err-owner-only)
    (map-set courses
      { course-id: course-id }
      (merge course { title: title, description: description, price: price })
    )
    (ok true)
  )
)

(define-public (buy-course-shares (course-id uint) (shares uint))
  (let
    (
      (course (unwrap! (map-get? courses { course-id: course-id }) err-not-found))
      (buyer tx-sender)
      (instructor (get instructor course))
      (price-per-share (/ (get price course) (get total-shares course)))
      (total-cost (* price-per-share shares))
    )
    (asserts! (<= shares (get available-shares course)) (err u103))
    (try! (stx-transfer? total-cost buyer instructor))
    (map-set courses
      { course-id: course-id }
      (merge course { available-shares: (- (get available-shares course) shares) })
    )
    (map-set course-ownership
      { course-id: course-id, owner: buyer }
      { shares: (default-to u0 (get shares (get-course-ownership course-id buyer))) }
    )
    (ok true)
  )
)

(define-public (transfer-course-shares (course-id uint) (recipient principal) (shares uint))
  (let
    (
      (sender tx-sender)
      (sender-shares (unwrap! (get-course-ownership course-id sender) err-not-found))
    )
    (asserts! (>= (get shares sender-shares) shares) (err u104))
    (map-set course-ownership
      { course-id: course-id, owner: sender }
      { shares: (- (get shares sender-shares) shares) }
    )
    (map-set course-ownership
      { course-id: course-id, owner: recipient }
      { shares: (+ (default-to u0 (get shares (get-course-ownership course-id recipient))) shares) }
    )
    (ok true)
  )
)
