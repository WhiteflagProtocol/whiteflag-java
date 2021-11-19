/*
 * Whiteflag Java Library
 */
package org.whiteflagprotocol.java.core;

import java.util.Map;
import java.util.Set;
import java.util.HashMap;
import java.util.HashSet;

/**
 * Whiteflag message segment class
 * 
 * <p> This is a class representing a segment of a Whiteflag message, such as a
 * the message header or the message body. A message segment contains a number of
 * message fields, depending on the part and the type of the message. The fields
 * should be ordered, without missing or overlapping bytes.
 * 
 * @wfref 4.2 Message Header
 * @wfref 4.3 Message Body
 */
public class WfMessageSegment {

    /* PROPERTIES */

    /**
     * Array of fields in this message segment
     */
    private WfMessageField[] fields;

    /**
     * Deserialisation, decoding and copying cursor
     */
    private int cursor = 0;

    /* CONSTRUCTOR */

    /**
     * Constructs a new Whiteflag message segment from an array of message fields,
     * without copying the fields' values
     * @param fields an array of message fields
     */
    public WfMessageSegment(final WfMessageField[] fields) {
        this.fields = new WfMessageField[fields.length];
        for (int i=0; i < fields.length; i++) {
            this.fields[i] = new WfMessageField(fields[i]);
        }
    }

    /**
     * Constructs a new Whiteflag message segment from another message segment,
     * also copying the fields' values
     * @param segment the message segment to create the new segment from
     */
    public WfMessageSegment(final WfMessageSegment segment) {
        this.fields = new WfMessageField[segment.getNoFields()];
        for (; cursor < this.fields.length; cursor++) {
            this.fields[cursor] = new WfMessageField(segment.getField(cursor));
            this.fields[cursor].set(segment.get(cursor));
        }
    }

    /* PUBLIC METHODS: basic object interface */

    /**
     * Returns the message segment as a concatinated string of field values
     * @return serialized message segment
     */
    @Override
    public final String toString() {
        String string;
        try {
            string = this.serialize();
        } catch (WfCoreException e) {
            return "";
        }
        return string; 
    }

    /* PUBLIC METHODS: metadata & validators */

    /**
     * Checks if all fields of this message segment contain valid data
     * @return TRUE if message segment contains valid data, else FALSE
     */
    public final Boolean isValid() {
        int byteCursor = fields[0].startByte;
        for (WfMessageField field : fields) {
            // Fields should be ordered without missing or overlapping bytes
            if (field.startByte != byteCursor) return false;
            byteCursor = field.endByte;
            // Field should be valid
            if (Boolean.FALSE.equals(field.isValid())) return false;
        }
        return true;
    }

    /**
     * Checks if the specified field contains valid data
     * @param fieldname the name of the field
     * @return TRUE if the field contains valid data, else FALSE
     */
    public final Boolean isValid(final String fieldname) {
        for (WfMessageField field : fields) {
            if (fieldname.equals(field.name)) {
                return field.isValid();
            }
        }
        return false;
    }

    /**
     * Checks if the provided data is valid for the specified field
     * @param fieldname the name of the field
     * @param data the value to be checked
     * @return TRUE if the field contains valid data, else FALSE
     */
    public final Boolean isValid(final String fieldname, final String data) {
        for (WfMessageField field : fields) {
            if (fieldname.equals(field.name)) {
                return field.isValid(data);
            }
        }
        return false;
    }

    /**
     * Gets the number of fields in this message segment
     * @return the number of message segment fields
     */
    public final int getNoFields() {
        return this.fields.length;
    }

    /**
     * Returns the field names of the message segment
     * @return a string set with all field names
     */
    public Set<String> getFieldNames() {
        Set<String> names = new HashSet<>();
        for (WfMessageField field : fields) {
            names.add(field.name);
        }
        return names;
    }

    /* PUBLIC METHODS: getters & setters */

    /**
     * Gets the value of the field specified by name
     * @param fieldname the name of the requested field
     * @return the field value, or NULL if field does not exist
     */
    public final String get(final String fieldname) {
        for (WfMessageField field : fields) {
            if (fieldname.equals(field.name)) return field.get();
        }
        return null;
    }

    /**
     * Gets the value of the field specified by index
     * @param index the index of the requested field; negative index counts back from last field
     * @return the field value, or NULL if it does not exist
     */
    public final String get(final int index) {
        if (index >= 0 && index < fields.length) {
            return fields[index].get();
        }
        if (index < 0 && (fields.length + index) >= 0) {
            return fields[fields.length + index].get();
        }
        return null;
    }

    /**
     * Sets the value of the specified field in the message segment
     * @param fieldname the name of the field
     * @param data data to be set as the field value
     * @return TRUE if field value is set, FALSE if field does not exits, isalready set, or data is invalid
     */
    public final Boolean set(final String fieldname, final String data) {
        for (WfMessageField field : fields) {
            if (fieldname.equals(field.name)) return field.set(data);
        }
        return false;
    }

    /**
     * Sets the value of the field specified by its index in the message segment
     * @param index the index of the requested field; negative index counts back from last field
     * @param data data to be set as the field value
     * @return TRUE if the data was valid and the field value is set, else FALSE
     */
    public final Boolean set(final int index, final String data) {
        if (index >= 0 && index < fields.length) {
            return fields[index].set(data);
        }
        if (index < 0 && (fields.length + index) >= 0) {
            return fields[fields.length + index].set(data);
        }
        return false;
    }

    /* PUBLIC METHODS: mapping */

    /**
     * Gets a fieldname-to-value mapping of this message segment
     * @return a fieldname-to-value mapping
     */
    public final Map<String, String> toMap() {
        Map<String, String> map = new HashMap<>(this.fields.length + 1, 1);
        for (WfMessageField field : fields) {
            map.put(field.name, field.get());
        }
        return map;
    }

    /**
     * Sets all field values of this segment from a fieldname-to-value mapping
     * @param map a fieldname-to-value mapping
     * @return TRUE if all field values in this segment were correctly set, else FALSE
    */
    public final Boolean setAll(final Map<String, String> map) {
        map.forEach(this::set);
        return this.isValid();
    }

    /**
     * Sets all field values of this segment with values from an array
     * @param data array with the data to be set as the field values
     * @param index starting position in the array
     * @return TRUE if the data was valid and all field values are set
     * @throws WfCoreException if the provided data is invalid
     */
    public final Boolean setAll(final String[] data, final int index) throws WfCoreException {
        int nFields = data.length - index;
        if (fields.length == 0 || nFields < fields.length) {
            throw new WfCoreException("Message segment has " + fields.length + " fields, but received data for " + nFields + " fields");
        }
        for (; cursor < this.fields.length; cursor++) {
            final int i = index + cursor;
            if (Boolean.FALSE.equals(fields[cursor].set(data[i]))) {
                throw new WfCoreException(fields[cursor].debugString() + " already set or invalid data in array at item " + i + ": " + data[i]);
            }
        }
        return this.isValid();
    }

    /* PROTECTED METHODS: operations */

    /**
     * Serializes this message segment
     * @return the serialized message segment
     * @throws WfCoreException if the segment cannot be serialized
     */
    protected final String serialize() throws WfCoreException {
        int byteCursor = fields[0].startByte;
        StringBuilder segmentStr = new StringBuilder();

        for (WfMessageField field : fields) {
            if (field.startByte != byteCursor) {
                throw new WfCoreException("Invalid field order while serializing: did not expect field " + field.name + " at byte " + byteCursor);
            }
            segmentStr.append(field.get());
            byteCursor = field.endByte;
        }
        return segmentStr.toString();
    }

    /**
     * Deserializes this message segment from the provided serialized message
     * @param messageStr the serialized message
     * @param startByte the byte position where this segment starts in the serialized message
     * @return the byte position where this segment ends in the serialized message
     */
    protected final int deserialize(final String messageStr, final int startByte) throws WfCoreException {
        int byteCursor = startByte;
        for (; cursor < this.fields.length; cursor++) {

            // Deserialize data
            String data;
            if (fields[cursor].endByte < 0) {
                // Undefined field length
                data = messageStr.substring(fields[cursor].startByte);
            } else {
                // Fixed field length
                data = messageStr.substring(fields[cursor].startByte, fields[cursor].endByte);
            }
            // Set the field value and check result
            if (Boolean.FALSE.equals(fields[cursor].set(data))) {
                throw new WfCoreException(fields[cursor].debugString() + " already set or invalid data in serialized message at byte " + byteCursor + ": " + data);
            }
            // Move to next field in serialized message
            byteCursor = fields[cursor].endByte;
        }
        return byteCursor;
    }

    /**
     * Encodes this message segment
     * @return the encoded message segment
     * @throws WfCoreException if the message cannot be encoded
     */
    protected final WfBinaryString encode() throws WfCoreException {
        int byteCursor = fields[0].startByte;
        WfBinaryString binSegment = new WfBinaryString();
        
        for (WfMessageField field : fields) {
            if (field.startByte != byteCursor) {
                throw new WfCoreException("Invalid field order while encoding: did not expect field " + field.name + " at byte " + byteCursor);
            }
            binSegment.append(field.encode());
            byteCursor = field.endByte;
        }
        return binSegment;
    }

    /**
     * Decodes this message segment from the provided encoded message
     * @param binMessage the binary encoded message
     * @param startBit the bit position where this segment starts in the encoded message
     * @return the bit position where this segment ends in the encoded message
     */
    protected final int decode(final WfBinaryString binMessage, final int startBit) throws WfCoreException {
        int bitCursor = startBit;
        for (; cursor < this.fields.length; cursor++) {
            final int endBit = bitCursor + fields[cursor].bitLength();

            // Decode data
            String data;
            if (fields[cursor].endByte < 0) {
                // Undefined field length
                data = fields[cursor].decode(binMessage.sub(bitCursor));
            } else {
                // Fixed field length
                data = fields[cursor].decode(binMessage.sub(bitCursor, endBit));
            }
            // Set the field value
            if (Boolean.FALSE.equals(fields[cursor].set(data))) {
                throw new WfCoreException(fields[cursor].debugString() + " already set or invalid data in encoded binary message at bit " + bitCursor + ": " + data);
            }
            // Move to next field in encoded message
            bitCursor = endBit;
        }
        return bitCursor;
    }

    /* PROTECTED METHODS: object operations */

    /**
     * Appends additional fields to this message segment if constructing complex message bodies
     * @param segment the segment to be added to the message segment
     * @return this {@link WfMessageSegment}
     */
    protected final WfMessageSegment append(final WfMessageSegment segment) {
        // Create new field array and fill with original fields from this segment
        WfMessageField[] newFields = new WfMessageField[this.getNoFields() + segment.getNoFields()];
        System.arraycopy(this.fields, 0, newFields, 0, this.fields.length);

        // Check ending and starting bytes of both segments
        WfMessageField endField = this.getField(-1);
        WfMessageField startField = segment.getField(0); 
        int shift = 0;
        if (endField != null && startField != null) {
            shift = endField.endByte - startField.startByte;
        }
        // Add new fields from other segment with shifted start and end byte to array
        int i = this.fields.length;
        for (WfMessageField field : segment.getAllFields()) {
            newFields[i] = new WfMessageField(field, shift);
            i++;
        }
        // Set the fields with new field array, update cursor if all fields are valid and return this object
        this.fields = newFields;
        if (cursor != 0 && Boolean.TRUE.equals(this.isValid())) {
            cursor = this.fields.length;
        }
        return this;
    }

    /**
     * Gets the field specified by name
     * @param fieldname the name of the requested field
     * @return the requested message field, or NULL if it does not exist
     */
    protected final WfMessageField getField(final String fieldname) {
        for (WfMessageField field : fields) {
            if (fieldname.equals(field.name)) return field;
        }
        return null;
    }

    /**
     * Gets the field specified by index
     * @param index the index of the requested field; negative index counts back from last field
     * @return the requested message field, or NULL if it does not exist
     */
    protected final WfMessageField getField(final int index) {
        if (index >= 0 && index < fields.length) {
            return fields[index];
        }
        if (index < 0 && (fields.length + index) >= 0) {
            return fields[fields.length + index];
        }
        return null;
    }

    /**
     * Gets all fields from this message segment
     * @return an array with all message fields
     */
    protected final WfMessageField[] getAllFields() {
        return this.fields;
    }
}
