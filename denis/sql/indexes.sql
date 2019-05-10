CREATE OR REPLACE FUNCTION my_array_to_text(text[])
  RETURNS text LANGUAGE sql IMMUTABLE AS $$SELECT array_to_string($1, ' ')$$;

CREATE INDEX "faculty_text" ON "faculties" USING GIN (to_tsvector('russian', "facultyName" || ' ' || "facultyPath" || ' ' || "facultyCampusName" || ' ' || my_array_to_text("facultyTags")))

