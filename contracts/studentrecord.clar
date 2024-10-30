;; BlockLearnAdemy Student Records Contract

;; Constants
(define-constant contract-owner tx-sender)
(define-constant err-owner-only (err u100))
(define-constant err-not-found (err u101))
(define-constant err-already-exists (err u102))
(define-constant err-invalid-input (err u103))
(define-constant err-not-enrolled (err u104))
(define-constant err-already-completed (err u105))

;; Define the principal of the course management contract
(define-data-var course-management-contract (optional principal) none)

;; Data Maps
(define-map student-enrollments
  { student: principal, course-id: uint }
  { enrolled-at: uint, completed-at: (optional uint), progress: uint }
)

(define-map student-achievements
  { student: principal }
  { courses-completed: (list 100 uint), total-credits: uint }
)

;; Read-only functions
(define-read-only (get-student-enrollment (student principal) (course-id uint))
  (map-get? student-enrollments { student: student, course-id: course-id })
)

(define-read-only (get-student-achievements (student principal))
  (default-to 
    { courses-completed: (list ), total-credits: u0 }
    (map-get? student-achievements { student: student })
  )
)

;; Private functions
(define-private (update-student-achievements (student principal) (course-id uint) (credits uint))
  (let
    (
      (current-achievements (get-student-achievements student))
      (updated-courses (unwrap! (as-max-len? (append (get courses-completed current-achievements) course-id) u100) err-invalid-input))
      (updated-credits (+ (get total-credits current-achievements) credits))
    )
    (ok (map-set student-achievements
      { student: student }
      {
        courses-completed: updated-courses,
        total-credits: updated-credits
      }
    ))
  )
)

;; Public functions
(define-public (set-course-management-contract (new-contract principal))
  (begin
    (asserts! (is-eq tx-sender contract-owner) err-owner-only)
    (var-set course-management-contract (some new-contract))
    (ok true)
  )
)

(define-public (enroll-student (student principal) (course-id uint))
  (begin
    (asserts! (is-none (get-student-enrollment student course-id)) err-already-exists)
    (ok (map-set student-enrollments
      { student: student, course-id: course-id }
      { enrolled-at: block-height, completed-at: none, progress: u0 }
    ))
  )
)

(define-public (update-progress (student principal) (course-id uint) (new-progress uint))
  (let
    (
      (enrollment (unwrap! (get-student-enrollment student course-id) err-not-enrolled))
    )
    (asserts! (< new-progress u101) err-invalid-input)
    (asserts! (is-none (get completed-at enrollment)) err-already-completed)
    (ok (map-set student-enrollments
      { student: student, course-id: course-id }
      (merge enrollment { progress: new-progress })
    ))
  )
)

(define-public (complete-course (student principal) (course-id uint))
  (let
    (
      (enrollment (unwrap! (get-student-enrollment student course-id) err-not-enrolled))
    )
    (asserts! (is-none (get completed-at enrollment)) err-already-completed)
    (try! (update-student-achievements student course-id u1))
    (ok (map-set student-enrollments
      { student: student, course-id: course-id }
      (merge enrollment { completed-at: (some block-height), progress: u100 })
    ))
  )
)

(define-read-only (get-student-progress (student principal) (course-id uint))
  (match (get-student-enrollment student course-id)
    enrollment (ok (get progress enrollment))
    (err err-not-enrolled)
  )
)

(define-read-only (get-completed-courses (student principal))
  (ok (get courses-completed (get-student-achievements student)))
)

(define-read-only (get-total-credits (student principal))
  (ok (get total-credits (get-student-achievements student)))
)
