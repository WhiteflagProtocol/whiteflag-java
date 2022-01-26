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
 * 
 * @since 1.0
 */
public class WfMessageSegment {

    /* PROPERTIES */

    /**
     * Array of fields in this message segment
     */
    private WfMessageField[] fields;

    /* CONSTRUCTORS */

    /**
     * Constructs a new Whiteflag message segment from an array of message fields,
     * without copying the fields' values
     * @param fields an array of message fields
     */
    public WfMessageSegment(final WfMessageField[] fields) {
        this.fields = new WfMessageField[fields.length];
        for (int i=0; i < fields.length; i++) {
            this.fields[i] = WfMessageField.from(fields[i]);
        }
    }

    /**
     * Constructs a new Whiteflag message segment from another message segment,
     * also copying the fields' values
     * @param segment the message segment to create the new segment from
     */
    public WfMessageSegment(final WfMessageSegment segment) {
        this.fields = new WfMessageField[segment.getNoFields()];
        for (int index = 0; index < this.fields.length; index++) {
            this.fields[index] = WfMessageField.from(segment.getField(index));
            this.fields[index].set(segment.get(index));
        }
    }

    /* PUBLIC METHODS */

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

    /**
     * Checks if all fields of this message segment contain valid data
     * @return TRUE if message segment contains valid data, else FALSE
     */
    public final boolean isValid() {
        int byteCursor = fields[0].startByte;
        for (WfMessageField field : fields) {
            /* Fields should be ordered without missing or overlapping bytes */
            if (field.startByte != byteCursor) return false;
            byteCursor = field.endByte;
            /* Field should be valid */
            if (Boolean.FALSE.equals(field.isValid())) return false;
        }
        return true;
    }

    /**
     * Checks if the specified field contains valid data
     * @param fieldname the name of the field
     * @return TRUE if the field contains valid data, else FALSE
     */
    public final boolean isValid(final String fieldname) {
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
    public final boolean isValid(final String fieldname, final String data) {
        for (WfMessageField field : fields) {
            if (fieldname.equals(field.name)) {
                return field.isValid(data);
            }
        }
        return false;
    }

    /**
     * Returns the bit length of this segment, excluding the last variable length field if not set
     * @return the bit length of this segment
     */
    public final int bitLength() {
        return bitLength(-1);
    }

    /**
     * Returns the bit length of this segment, excluding the last variable length field if not set
     * @param fieldName the name of the field up to which the segment length is calculated; negative index counts back from last field
     * @return the bit length of this segment, or 0 if the field does not exist
     */
    public final int bitLength(final String fieldName) {
        return bitLength(getFieldIndex(fieldName));
    }

    /**
     * Returns the bit length up to and including the specified field, excluding the last variable length field if not set
     * @param fieldIndex the index of the field up to which the segment length is calculated; negative index counts back from last field
     * @return the bit length of this segment up to and including the specified field, or 0 if the field does not exist
     */
    public final int bitLength(final int fieldIndex) {
        /* Check provided index */
        final int lastFieldIndex = getAbsoluteIndex(fieldIndex);
        if (lastFieldIndex < 0) return 0;

        /* Calculate segment bit length */
        int bitLength = 0;
        for (int index = 0; index <= lastFieldIndex; index++) {
            bitLength += fields[index].bitLength();
        }
        return bitLength;
    }

    /**
     * Returns the byte length of this segment, excluding the last variable length field if not set
     * @return the byte length of this segment
     */
    public final int byteLength() {
        return byteLength(-1);
    }

    /**
     * Returns the byte length of this segment, excluding the last variable length field if not set
     * @param fieldName the name of the field up to which the segment length is calculated; negative index counts back from last field
     * @return the byte length of this segment, or 0 if the field does not exist
     */
    public final int byteLength(final String fieldName) {
        return byteLength(getFieldIndex(fieldName));
    }

    /**
     * Returns the byte length of this segment up to and including the specified field, excluding the last variable length field if not set
     * @param fieldIndex the index of the field up to which the segment length is calculated; negative index counts back from last field
     * @return the byte length of this segment, , or 0 if the field does not exist
     */
    @SuppressWarnings("java:S2259")
    public int byteLength(final int fieldIndex) {
        /* Check provided index */
        final int lastFieldIndex = getAbsoluteIndex(fieldIndex);
        if (lastFieldIndex < 0) return 0;
        
        /* Calculate segment byte length */
        if (this.fields.length == 0) return 0;
        return getField(fieldIndex).endByte - getField(0).startByte;
    }

    /**
     * Gets the number of fields in this message segment
     * @return the number of message segment fields
     */
    public final int getNoFields() {
        return +this.fields.length;
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
     * @param fieldIndex the index of the requested field; negative index counts back from last field
     * @return the field value, or NULL if field does not exist
     */
    public final String get(final int fieldIndex) {
        final int index = getAbsoluteIndex(fieldIndex);
        if (index < 0) return null;
        return fields[index].get();
    }

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
     * @param fieldIndex the index of the requested field; negative index counts back from last field
     * @param data data to be set as the field value
     * @return TRUE if the data was valid and the field value is set, else FALSE
     */
    public final Boolean set(final int fieldIndex, final String data) {
        final int index = getAbsoluteIndex(fieldIndex);
        if (index < 0) return false;
        return fields[index].set(data);
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
     * @since 1.1
     * @param data array with the data to be set as the field values
     * @return TRUE if the data was valid and all field values are set
     * @throws WfCoreException if the provided data is invalid
     */
    public final Boolean setAll(final String[] data) throws WfCoreException {
        return setAll(data, 0);
    }

    /**
     * Sets all field values of this segment with values from an array
     * @since 1.1
     * @param data array with the data to be set as the field values
     * @param startIndex starting position in the array
     * @return TRUE if the data was valid and all field values are set
     * @throws WfCoreException if the provided data is invalid
     */
    public final Boolean setAll(final String[] data, final int startIndex) throws WfCoreException {
        /* Check if data array contains data for all fields */
        int nItems = data.length - startIndex;
        if (nItems < fields.length) {
            throw new WfCoreException("Message segment has " + fields.length + " fields, but received " + nItems + " items in array", null);
        }
        /* Set all fields */
        int index = startIndex;
        for (WfMessageField field : fields) {
            if (Boolean.FALSE.equals(field.set(data[index]))) {
                throw new WfCoreException("Field " + field.debugInfo() + " already set or array item " + index + " contains invalid data: " + data[index], null);
            }
            index++;
        }
        return this.isValid();
    }

    /* PROTECTED METHODS */

    /**
     * Serializes this message segment
     * @return the serialized message segment
     * @throws WfCoreException if the segment cannot be serialized
     */
    @SuppressWarnings("java:S1192")
    protected final String serialize() throws WfCoreException {
        int byteCursor = fields[0].startByte;
        StringBuilder segmentStr = new StringBuilder();
        for (WfMessageField field : fields) {
            if (field.startByte != byteCursor) {
                throw new WfCoreException("Invalid field order while serializing: did not expect field " + field.debugInfo() + " at byte " + byteCursor, null);
            }
            segmentStr.append(field.get());
            byteCursor = field.endByte;
        }
        return segmentStr.toString();
    }

    /**
     * Deserializes this message segment from the provided serialized message
     * @since 1.1
     * @param messageStr the serialized message
     * @param fieldIndex the index of the next field to be deserialized
     * @throws WfCoreException if the message cannot be deserialized
     */
    @SuppressWarnings("java:S1192")
    protected final void deserialize(final String messageStr, final int fieldIndex) throws WfCoreException {
        /* Check if all fields already processed */
        if (fieldIndex >= this.fields.length) return;

        /* Continue deserialization of fields */
        int byteCursor = fields[fieldIndex].startByte;
        for (int index = fieldIndex; index < this.fields.length; index++) {
            if (fields[index].startByte != byteCursor) {
                throw new WfCoreException("Invalid field order while deserializing: did not expect field " + fields[index].debugInfo() + " at byte " + byteCursor, null);
            }
            String data;
            if (fields[index].endByte < 0) {
                data = messageStr.substring(fields[index].startByte);
            } else {
                data = messageStr.substring(fields[index].startByte, fields[index].endByte);
            }
            /* Set field value */
            if (Boolean.FALSE.equals(fields[index].set(data))) {
                throw new WfCoreException("Could not set field " + fields[index].debugInfo() + " with deserialized data at byte " + byteCursor + ": " + data, null);
            }
            byteCursor = fields[index].endByte;
        }
    }

    /**
     * Encodes this message segment
     * @since 1.1
     * @return a binary buffer with the binary encoded message segment
     * @throws WfCoreException if the message cannot be encoded
     */
    @SuppressWarnings("java:S1192")
    protected final WfBinaryBuffer encode() throws WfCoreException {
        WfBinaryBuffer buffer = WfBinaryBuffer.create();
        int byteCursor = fields[0].startByte;
        for (WfMessageField field : fields) {
            if (field.startByte != byteCursor) {
                throw new WfCoreException("Invalid field order while encoding: did not expect field " + field.debugInfo() + " at byte " + byteCursor, null);
            }
            buffer.addMessageField(field);
            byteCursor = field.endByte;
        }
        return buffer;
    }

    /**
     * Decodes this message segment from the provided encoded message
     * @since 1.1
     * @param buffer the binary buffer with the binary encoded message
     * @param startBit the bit position where this segment starts in the encoded message
     * @param fieldIndex the index of the next field to be decoded
     * @throws WfCoreException if the message cannot be decoded
     */
    @SuppressWarnings("java:S1192")
    protected final void decode(final WfBinaryBuffer buffer, final int startBit, final int fieldIndex) throws WfCoreException {
        /* Check if all fields already processed */
        if (fieldIndex >= this.fields.length) return;

        /* Continue deserialization of fields */
        int bitCursor = startBit;
        int byteCursor = fields[fieldIndex].startByte;
        for (int index = fieldIndex; index < this.fields.length; index++) {
            if (fields[index].startByte != byteCursor) {
                throw new WfCoreException("Invalid field order while decoding: did not expect field " + fields[index].debugInfo() + " at byte " + byteCursor, null);
            }
            try {
                buffer.extractMessageField(fields[index], bitCursor);
            } catch (WfCoreException e) {
                throw new WfCoreException("Could not decode field at bit " + bitCursor + " of buffer: " + buffer.toHexString(), e);
            }
            bitCursor += fields[index].bitLength();
            byteCursor = fields[index].endByte;
        }
    }

    /**
     * Appends additional fields to this message segment if constructing complex message bodies
     * @param segment the segment to be added to the message segment
     * @return this {@link WfMessageSegment}
     */
    protected final WfMessageSegment append(final WfMessageSegment segment) {
        /* Create new field array and fill with original fields from this segment */
        WfMessageField[] newFields = new WfMessageField[this.getNoFields() + segment.getNoFields()];
        System.arraycopy(this.fields, 0, newFields, 0, this.fields.length);

        /* Check ending and starting bytes of both segments */
        WfMessageField endField = this.getField(-1);
        WfMessageField startField = segment.getField(0); 
        int shift = 0;
        if (endField != null && startField != null) {
            shift = endField.endByte - startField.startByte;
        }
        /* Add new fields from other segment with shifted start and end byte to array */
        int i = this.fields.length;
        for (WfMessageField field : segment.getAllFields()) {
            newFields[i] = WfMessageField.from(field, shift);
            i++;
        }
        /* Set the fields with new field array */
        this.fields = newFields;
        return this;
    }

    /**
     * Gets the field specified by name
     * @param fieldName the name of the requested field
     * @return the requested message field, or NULL if it does not exist
     */
    protected final WfMessageField getField(final String fieldName) {
        for (WfMessageField field : fields) {
            if (fieldName.equals(field.name)) return field;
        }
        return null;
    }

    /**
     * Gets the index of the field specified by name
     * @param fieldName the name of the requested field
     * @return the index of the requested field, or the number of fields if the specified field does not exist
     */
    protected final int getFieldIndex(final String fieldName) {
        int fieldIndex = 0;
        for (WfMessageField field : fields) {
            if (fieldName.equals(field.name)) return fieldIndex;
            fieldIndex++;
        }
        return fieldIndex;
    }

    /**
     * Gets the field specified by index
     * @param fieldIndex the index of the requested field; negative index counts back from last field
     * @return the requested message field, or NULL if it does not exist
     */
    protected final WfMessageField getField(final int fieldIndex) {
        final int index = getAbsoluteIndex(fieldIndex);
        if (index < 0) return null;
        return fields[index];
    }

    /**
     * Gets all fields from this message segment
     * @return an array with all message fields
     */
    protected final WfMessageField[] getAllFields() {
        return this.fields;
    }

    /* PRIVATE METHODS */

    /**
     * Gets the absolute field index and check if index is within bounds
     * @param index the absolute index of the requested field; negative index counts back from last field
     * @return the absolute field index or -1 if index out of bounds
     */
    private final int getAbsoluteIndex(final int index) {
        if (index >= 0 && index < fields.length) {
            return index;
        }
        if (index < 0 && (fields.length + index) >= 0) {
            return fields.length + index;
        }
        return -1;
    }
}
